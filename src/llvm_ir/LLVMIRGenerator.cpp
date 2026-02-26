#include "llvm_ir/LLVMIRGenerator.h"

/*=============================================================*/
/*Utility functions for generating inline assembly for syscalls*/
/*=============================================================*/

std::string generateLinuxARM64InlineAsm(size_t argCount, int syscallNumber) {
  std::ostringstream asmCode;

  // Move syscall args into x0–x5
  for (size_t i = 0; i < argCount; ++i) {
    asmCode << "mov x" << i << ", $" << i << "\n";
  }

  asmCode << "mov x8, #" << syscallNumber << "\n";
  asmCode << "svc #0\n";
  return asmCode.str();
}

std::string generateMacOSInlineAsm(size_t argCount, int syscallNumber) {
  std::ostringstream asmCode;
  for (size_t i = 0; i < argCount; ++i) {
    asmCode << "mov x" + std::to_string(i) + ", $" + std::to_string(i) + "\n";
  }

  asmCode << "mov x16, #" + std::to_string(syscallNumber & 0xFFFF) + "\n";
  asmCode << "orr x16, x16, #0x2000000\n";
  asmCode << "svc #0x80\n";
  return asmCode.str();
}

std::string generateConstraints(size_t argCount) {
  std::string constraints;
  for (size_t i = 0; i < argCount; ++i) {
    constraints += "r";
    if (i != argCount - 1)
      constraints += ",";
  }
  return constraints;
}

// syscall builder class
llvm::Value* SyscallBuilder::emitSyscall(
    llvm::IRBuilder<>& builder,
    llvm::LLVMContext& context,
    llvm::Module* module,
    const std::vector<llvm::Value*>&
        args, // args = {syscall_number, arg0, ..., arg5}
    uint64_t /*unused*/,
    TargetABI targetABI) {
  assert(args.size() <= 7 && "Too many arguments including syscall number");

  std::vector<llvm::Type*> argTypes(
      args.size(), llvm::Type::getInt64Ty(context));
  llvm::FunctionType* funcType =
      llvm::FunctionType::get(llvm::Type::getInt64Ty(context), argTypes, false);

  std::string asmString;
  std::string constraints;

  if (targetABI == TargetABI::MacOS_ARM64) {
    // x16 = syscall number, x0-x5 = args
    // InlineAsm assigns: $0 = x16, $1 = x0, ..., $6 = x5
    asmString = "mov x16, $0\n"; // syscall number
    for (size_t i = 1; i < args.size(); ++i)
      asmString +=
          "mov x" + std::to_string(i - 1) + ", $" + std::to_string(i) + "\n";
    asmString += "svc #0x80\n";
    asmString += "mov $0, x0"; // return x0

    constraints = "=r"; // return
    for (size_t i = 0; i < args.size(); ++i)
      constraints += ",r";
  } else if (targetABI == TargetABI::Linux_ARM64) {
    // x8 = syscall number, x0-x5 = args
    asmString = "mov x8, $0\n";
    for (size_t i = 1; i < args.size(); ++i)
      asmString +=
          "mov x" + std::to_string(i - 1) + ", $" + std::to_string(i) + "\n";
    asmString += "svc #0\n";
    asmString += "mov $0, x0";

    constraints = "=r";
    for (size_t i = 0; i < args.size(); ++i)
      constraints += ",r";
  } else {
    llvm::errs() << "Unsupported ABI for syscall\n";
    return nullptr;
  }

  llvm::InlineAsm* inlineAsm =
      llvm::InlineAsm::get(funcType, asmString, constraints, true);
  return builder.CreateCall(inlineAsm, args, "syscall_result");
}

void SyscallBuilder::emitExitSyscall(
    llvm::IRBuilder<>& builder,
    llvm::LLVMContext& context,
    llvm::Module* module,
    llvm::Value* status,
    TargetABI targetABI) {
  uint64_t exitNumber = (targetABI == TargetABI::MacOS_ARM64) ? 0x2000001 : 93;
  llvm::Value* syscallNumVal =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), exitNumber);

  llvm::Value* result = emitSyscall(
      builder, context, module, {syscallNumVal, status}, 0, targetABI);
  (void)result;
}

llvm::Value* SyscallBuilder::emitGenericSyscall(
    llvm::IRBuilder<>& builder,
    llvm::LLVMContext& context,
    llvm::Module* module,
    llvm::Value* syscallNumber,
    llvm::Value* args[],
    TargetABI targetABI) {
  if (targetABI == TargetABI::MacOS_ARM64) {
    llvm::Function* convertFunc =
        module->getFunction("convert_x86_to_macos_syscall");
    if (!convertFunc) {
      llvm::FunctionType* funcType = llvm::FunctionType::get(
          llvm::Type::getInt64Ty(context),
          {llvm::Type::getInt64Ty(context)},
          false);

      convertFunc = llvm::Function::Create(
          funcType,
          llvm::Function::ExternalLinkage,
          "convert_x86_to_macos_syscall",
          module);

      llvm::BasicBlock* entry =
          llvm::BasicBlock::Create(context, "entry", convertFunc);
      llvm::IRBuilder<> convBuilder(entry);

      llvm::Argument* arg = &*convertFunc->arg_begin(); // syscall_number
      llvm::Value* offset =
          llvm::ConstantInt::get(context, llvm::APInt(64, 0x2000000));
      llvm::Value* result = convBuilder.CreateAdd(arg, offset);
      convBuilder.CreateRet(result);
    }

    syscallNumber = builder.CreateCall(convertFunc, {syscallNumber});
  }

  std::vector<llvm::Value*> fullArgs = {syscallNumber};
  for (int i = 0; i < 6; ++i) {
    fullArgs.push_back(
        args[i] ? args[i]
                : llvm::ConstantInt::get(context, llvm::APInt(64, 0)));
  }

  return emitSyscall(builder, context, module, fullArgs, 0, targetABI);
}

/*=============================================================*/
/*              Other utility functions*                       */
/*=============================================================*/

// Loading or getting the register
llvm::Value* LLVMIRGen::getOrLoadRegister(const std::string& regName) {
  llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();
  llvm::Value* regPtr = getNamedValue(regName);
  if (!regPtr) {
    std::cerr << "Error: Source register " << regName << " not found "
              << "Allocating it.\n";
    regPtr =
        builder.CreateAlloca(llvm::Type::getInt32Ty(context), nullptr, regName);
    builder.CreateStore(
        llvm::ConstantInt::get(context, llvm::APInt(32, 0)), regPtr);
    namedValues[regName] = regPtr;
  }
  return builder.CreateLoad(
      llvm::Type::getInt32Ty(context), regPtr, regName + "_load");
}

// Handling memory node
llvm::Value* LLVMIRGen::getOrLoadMemory(MemoryNode* memNode) {
  llvm::Value* baseAddr = nullptr;
  llvm::Value* offsetVal = nullptr;
  if (!memNode->base.empty()) {
    // check if it is a predefined global variable
    // and create it if not found, for some cases you can find
    // it in the .bss section which defines the uninitialized data section
    if (!getNamedValue(memNode->base)) {
      std::cerr << "Warning: Base register '" << memNode->base
                << "' not allocated. Allocating now.\n";
      // initialize the global variable
      llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
          module,
          llvm::Type::getInt32Ty(context),
          false,
          llvm::GlobalValue::ExternalLinkage,
          llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
          memNode->base);
      globalNamedValues[memNode->base] = globalVar; // registered globally
    }
    baseAddr = getNamedValue(memNode->base);
    baseAddr = builder.CreateLoad(
        llvm::Type::getInt32Ty(context), baseAddr, memNode->base);
  }
  if (!memNode->offset.empty()) {
    try {
      int offsetInt = std::stoi(memNode->offset);
      offsetVal =
          llvm::ConstantInt::get(context, llvm::APInt(32, offsetInt, true));
    } catch (const std::exception& e) {
      std::cerr << "Error: Invalid memory offset '" << memNode->offset << "'\n";
      return nullptr;
    }
  }
  llvm::Value* memAddr = baseAddr;
  if (offsetVal) {
    memAddr = builder.CreateAdd(baseAddr, offsetVal, "mem_addr");
  }
  llvm::Type* i32PtrType =
      llvm::PointerType::get(llvm::Type::getInt32Ty(context), 0);
  llvm::Value* ptr = builder.CreateIntToPtr(memAddr, i32PtrType, "ptr_cast");
  return ptr;
}

// Input type casting
llvm::Value* LLVMIRGen::castInputTypes(
    ASTNode* inputNode,
    const std::string nodeType) {
  llvm::Value* nodeValue = nullptr;

  if (nodeType == "int") {
    auto* intLit = dynamic_cast<IntLiteralNode*>(inputNode);
    nodeValue =
        llvm::ConstantInt::get(context, llvm::APInt(32, intLit->value, true));
  } else if (nodeType == "reg") {
    auto* srcReg = dynamic_cast<RegisterNode*>(inputNode);
    std::string regName = srcReg->registerName;
    if (RegisterAlias.count(regName))
      regName = RegisterAlias[regName];
    nodeValue = getOrLoadRegister(regName);

    if (!nodeValue) {
      std::cerr << "Error: Register " << regName << " not found\n";
      return nullptr;
    }
  } else if (nodeType == "mem") {
    auto* memNode = dynamic_cast<MemoryNode*>(inputNode);
    if (!memNode) {
      std::cerr << "Error: Invalid memory operand\n";
      return nullptr;
    }
    llvm::Value* ptr = getOrLoadMemory(memNode);
    nodeValue =
        builder.CreateLoad(llvm::Type::getInt32Ty(context), ptr, "mem_load");
  } else if (nodeType == "str") {
    auto* strNode = dynamic_cast<StringNode*>(inputNode);
    if (!strNode || strNode->value.empty()) {
      std::cerr << "Error: Invalid string operand\n";
      return nullptr;
    }

    // Get the ASCII value of the first character in the string
    char firstChar = strNode->value[0];
    int asciiValue = static_cast<int>(firstChar);

    // Create an LLVM constant for the ASCII value
    nodeValue =
        llvm::ConstantInt::get(context, llvm::APInt(32, asciiValue, true));
  } else {
    std::cerr << "Error: Invalid operand type for casting: " << nodeType
              << "\n";
    inputNode->print();
    return nullptr;
  }
  return nodeValue;
}

// Log the function call
llvm::FunctionCallee LLVMIRGen::getPrintFunction() {
  llvm::FunctionType* ft = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(context),
      llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context)),
      false);

  return module.getOrInsertFunction("puts", ft);
}

// take a snapshot of general purpose registers before entering a function call
void LLVMIRGen::snapshotRegisterState(llvm::Function* func) {
  std::vector<std::string> regs = {"eax", "ebx", "ecx", "edx", "esi", "edi"};
  for (const auto& reg : regs) {
    llvm::Value* regPtr = getNamedValue(reg);
    if (!regPtr || !regPtr->getType()->isPointerTy())
      continue;

    llvm::Value* val = builder.CreateLoad(
        llvm::Type::getInt32Ty(context), regPtr, reg + "_val");
    llvm::AllocaInst* shadow = builder.CreateAlloca(
        llvm::Type::getInt32Ty(context), nullptr, reg + "_shadow");
    builder.CreateStore(val, shadow);
    namedValues[reg + "_shadow"] = shadow;
  }
}
// restore the register state after the function call
void LLVMIRGen::restoreRegisterState(llvm::Function* func) {
  std::vector<std::string> regs = {"eax", "ebx", "ecx", "edx", "esi", "edi"};
  for (const auto& reg : regs) {
    if (!namedValues.count(reg + "_shadow"))
      continue;

    llvm::Value* shadowPtr = getNamedValue(reg + "_shadow");
    llvm::Value* regPtr = getNamedValue(reg);
    if (!regPtr || !regPtr->getType()->isPointerTy()) {
      regPtr =
          builder.CreateAlloca(llvm::Type::getInt32Ty(context), nullptr, reg);
      namedValues[reg] = regPtr;
    }
    llvm::Value* val = builder.CreateLoad(
        llvm::Type::getInt32Ty(context), shadowPtr, reg + "_restore");
    builder.CreateStore(val, regPtr);

    namedValues.erase(reg + "_shadow");
  }
}
// stack memory snapshot
void LLVMIRGen::snapshotStackState(llvm::Function* func) {
  if (!rspIntPerFunction.count(func) || !stackMemIntPerFunction.count(func))
    return;
  llvm::AllocaInst* savedRsp = builder.CreateAlloca(
      llvm::Type::getInt32Ty(context), nullptr, "rsp_shadow");
  llvm::AllocaInst* savedRspPtr = builder.CreateAlloca(
      llvm::Type::getInt32Ty(context), nullptr, "rsp_ptr_shadow");

  llvm::Value* valRsp = builder.CreateLoad(
      llvm::Type::getInt32Ty(context), rspIntPerFunction[func]);
  llvm::Value* valRspPtr = builder.CreateLoad(
      llvm::Type::getInt32Ty(context), rspPtrPerFunction[func]);

  builder.CreateStore(valRsp, savedRsp);
  builder.CreateStore(valRspPtr, savedRspPtr);

  namedValues["rsp_shadow"] = savedRsp;
  namedValues["rsp_ptr_shadow"] = savedRspPtr;
}

// restore the stack state after the function call
void LLVMIRGen::restoreStackState(llvm::Function* func) {
  if (!namedValues.count("rsp_shadow") || !namedValues.count("rsp_ptr_shadow"))
    return;

  llvm::Value* valRsp = builder.CreateLoad(
      llvm::Type::getInt32Ty(context), getNamedValue("rsp_shadow"));
  llvm::Value* valRspPtr = builder.CreateLoad(
      llvm::Type::getInt32Ty(context), getNamedValue("rsp_ptr_shadow"));

  builder.CreateStore(valRsp, rspIntPerFunction[func]);
  builder.CreateStore(valRspPtr, rspPtrPerFunction[func]);

  namedValues.erase("rsp_shadow");
  namedValues.erase("rsp_ptr_shadow");
}

/*===================================================================*/
/*                    LLVMIRGen class implementation                 */
/*===================================================================*/

// This module object is used to store all the IR generated
// It is a container for all the IR instructions
llvm::Module* LLVMIRGen::generateIR(std::unique_ptr<ASTNode>& root) {
  for (const auto& child : root->children) {
    if (auto* var = dynamic_cast<GlobalVariableNode*>(child.get())) {
      visitGlobalVariableNode(var);
    } else if (auto* bss = dynamic_cast<BssNode*>(child.get())) {
      visitBssNode(bss);
    } else if (auto* func = dynamic_cast<FunctionNode*>(child.get())) {
      visitFunctionNode(func);
    }
  }
  return &module;
}

void LLVMIRGen::visitBssNode(BssNode* node) {
  llvm::Type* i8Type = llvm::Type::getInt8Ty(context);
  llvm::ArrayType* arrayType = nullptr;
  if (node->directiveName == "resb") {
    arrayType = llvm::ArrayType::get(i8Type, stoi(node->size));
  } else if (node->directiveName == "resw") {
    arrayType =
        llvm::ArrayType::get(llvm::Type::getInt16Ty(context), stoi(node->size));
  } else if (node->directiveName == "resd") {
    arrayType =
        llvm::ArrayType::get(llvm::Type::getInt32Ty(context), stoi(node->size));
  } else {
    throw std::runtime_error("Unsupported directive: " + node->directiveName);
  }
  // Create a global variable for the BSS section
  llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
      module,
      arrayType,
      false, // isConstant
      llvm::GlobalValue::ExternalLinkage,
      llvm::ConstantAggregateZero::get(arrayType), // zero-initialized
      node->varName);

  // Optionally add pointer to first element into named map
  llvm::Value* globalPtr = builder.CreateConstGEP2_32(
      arrayType, globalVar, 0, 0, node->varName + "_ptr");
  globalNamedValues[node->varName] = globalPtr;
}

void LLVMIRGen::visitGlobalVariableNode(GlobalVariableNode* node) {
  llvm::Type* i8Type = llvm::Type::getInt8Ty(context);
  llvm::ArrayType* arrayType =
      llvm::ArrayType::get(i8Type, node->value.size() + 1);
  llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
      module,
      arrayType,
      false,
      llvm::GlobalValue::ExternalLinkage,
      nullptr,
      node->name);

  llvm::Constant* init =
      llvm::ConstantDataArray::getString(context, node->value, true);
  globalVar->setInitializer(init);

  // add global vatiable to namedValues
  llvm::Value* globalPtr =
      builder.CreateConstGEP2_32(arrayType, globalVar, 0, 0, "global_ptr");
  globalNamedValues[node->name] = globalPtr;
}

void LLVMIRGen::visitFunctionNode(FunctionNode* node) {
  llvm::Function* function = nullptr;
  // Check if the function is already declared
  if (definedFunctionsMap.find(node->name) != definedFunctionsMap.end()) {
    function = definedFunctionsMap[node->name];
  } else {
    // Create the function if it hasn't been declared yet
    llvm::FunctionType* funcType =
        llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);
    function = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, node->name, &module);
    definedFunctionsMap[node->name] = function;
  }

  // set the named values for this context used by stack
  namedValues.clear();

  llvm::BasicBlock* entryBB = nullptr;
  bool isFirstBlock = true;
  std::vector<BasicBlockNode*> labelVector;
  for (const auto& function : Parser::parserLabelMap) {
    std::string functionName = function.first;
    if (functionName == node->name) {
      labelVector = function.second;
    }
  }
  for (size_t i = 0; i < labelVector.size(); ++i) {
    const std::string& labelName = labelVector[i]->label;
    if (labelMap.find(labelName) == labelMap.end()) {
      // Create a new basic block for the label and add it to the labelMap
      llvm::BasicBlock* labelBB =
          llvm::BasicBlock::Create(context, labelName, function);
      if (isFirstBlock) {
        entryBlockNames[labelName] = "entry";
        entryBB = labelBB;
        entryBB->setName("entry"); // Rename to "entry" for clarity
        builder.SetInsertPoint(entryBB);
        isFirstBlock = false;
        labelMap["entry"] = labelBB;
      }
      if (!isFirstBlock) {
        labelMap[labelName] = labelBB;
        entryBlockNames[labelName] = labelName;
      }
    }
  }

  // Set the entry block
  if (labelMap.find("entry") != labelMap.end()) {
    entryBB = labelMap["entry"];
    builder.SetInsertPoint(entryBB);
    // initialize function stack
    if (!rspIntPerFunction.count(function)) {
      initializeFunctionStack(function);
    }
  }

  std::vector<llvm::BasicBlock*> orderedBlocks;
  for (const auto& block : node->basicBlocks) {
    if (block) {
      auto* basicBlockNode = dynamic_cast<BasicBlockNode*>(block.get());
      std::string basicBlockName = entryBlockNames[basicBlockNode->label];

      if (labelMap.find(basicBlockName) != labelMap.end()) {
        llvm::BasicBlock* bb = labelMap[basicBlockName];
        orderedBlocks.push_back(bb); // Maintain correct order
      }
    }
  }

  // Visit each block and generate the IR
  for (size_t i = 0; i < node->basicBlocks.size(); ++i) {
    auto* block = node->basicBlocks[i].get();
    auto* basicBlockNode = dynamic_cast<BasicBlockNode*>(block);
    std::string basicBlockName = entryBlockNames[basicBlockNode->label];

    if (labelMap.find(basicBlockName) != labelMap.end()) {
      llvm::BasicBlock* bb = labelMap[basicBlockName];
      builder.SetInsertPoint(bb);
      visitBasicBlockNode(basicBlockNode, orderedBlocks);
    } else {
      std::cerr << "Error: Block '" << basicBlockNode->label
                << "' not found in labelMap.\n";
    }
  }

  // Ensure the function has a terminating instruction
  if (!builder.GetInsertBlock()->getTerminator()) {
    builder.CreateRetVoid();
  }
}

llvm::BasicBlock* inferNextBlock(
    llvm::BasicBlock* current,
    const std::vector<llvm::BasicBlock*>& blocks) {
  for (size_t i = 0; i < blocks.size() - 1; ++i) {
    if (blocks[i] == current) {
      return blocks[i + 1]; // Return next block in sequence
    }
  }
  return nullptr; // No next block found (end of function)
}

void LLVMIRGen::visitBasicBlockNode(
    BasicBlockNode* node,
    const std::vector<llvm::BasicBlock*>& orderedBlocks) {
  llvm::Function* function = builder.GetInsertBlock()->getParent();
  if (!function) {
    std::cerr << "Error: No parent function for the basic block\n";
    return;
  }

  // Get the block from the labelMap (it should exist from the initial
  // declaration step)
  std::string blockName = entryBlockNames[node->label];
  llvm::BasicBlock* bb = labelMap[blockName];
  if (!bb) {
    std::cerr << "Error: Block '" << blockName << "' not found in labelMap\n";
    return;
  }

  // Set the insertion point to the retrieved block
  builder.SetInsertPoint(bb);

  // Generate IR for instructions in this block
  for (const auto& instr : node->instructions) {
    visitInstructionNode(dynamic_cast<InstructionNode*>(instr.get()));
  }
  if (!bb->getTerminator()) {
    if (auto next = inferNextBlock(bb, orderedBlocks)) {
      builder.CreateBr(next);
    } else {
      builder.CreateRetVoid(); // Final block
    }
  }
}

// this helps to understand the type of operand
std::string check_operandType(ASTNode* operand) {
  if (auto* reg = dynamic_cast<RegisterNode*>(operand)) {
    return "reg";
  } else if (auto* mem = dynamic_cast<MemoryNode*>(operand)) {
    return "mem";
  } else if (auto* intLit = dynamic_cast<IntLiteralNode*>(operand)) {
    return "int";
  } else if (auto* string = dynamic_cast<StringNode*>(operand)) {
    return "str";
  }
  return "unknown";
}

void LLVMIRGen::visitInstructionNode(InstructionNode* node) {
  if (node->opcode == "mov") {
    handleMovInstructionNode(node);
    return;
  } else if (node->opcode == "lea") {
    handleLeaInstructionNode(node);
    return;
  } else if (node->opcode == "int") {
    handleIntInstructionNode(node);
    return;
  } else if (node->opcode == "call") {
    handleCallInstructionNode(node);
    return;
  } else if (node->opcode == "div") {
    handleDivInstructionNode(node);
    return;
  } else if (node->opcode == "ret") {
    handleRetInstructionNode(node);
    return;
  } else if (node->opcode == "inc") {
    handleUnaryInstructionNode(node, "inc");
    return;
  } else if (node->opcode == "dec") {
    handleUnaryInstructionNode(node, "dec");
    return;
  } else if (node->opcode == "neg") {
    handleUnaryInstructionNode(node, "neg");
  } else if (node->opcode == "push" || node->opcode == "pop") {
    handleStackOperationInstructionNode(node);
    return;
  } else if (binOpTable.find(node->opcode) != binOpTable.end()) {
    handleBinaryOpNode(node);
    return;
  } else if (cmpOpTable.find(node->opcode) != cmpOpTable.end()) {
    handleCompareInstructionNode(node);
    return;
  } else if (jumpOpTable.find(node->opcode) != jumpOpTable.end()) {
    handleBranchingInstructions(node);
    return;
  } else if (node->opcode == "end") {
    // NASM end-of-file marker — no IR to emit
    return;
  } else {
    std::cerr << "Unsuppoerted operand type: " << node->opcode << "\n";
    return;
  }
}

void LLVMIRGen::handleBinaryOpNode(InstructionNode* node) {
  ASTNode* lhsNode = nullptr;
  ASTNode* rhsNode = nullptr;

  if (node->operands.size() == 2) {
    lhsNode = node->operands[0].get();
    rhsNode = node->operands[1].get();
  } else if (node->operands.size() == 3) {
    lhsNode = node->operands[1].get();
    rhsNode = node->operands[2].get();
  } else if (node->operands.size() == 4) {
    lhsNode = node->operands[0].get();
    rhsNode = node->operands[1].get();
  } else {
    std::cerr << "Error: Unexpected operand count for '" << node->opcode
              << "'\n";
    return;
  }

  if (!lhsNode || !rhsNode) {
    std::cerr << "Error: Invalid operands for binary operation\n";
    return;
  }

  std::string lhsType = check_operandType(lhsNode);
  std::string rhsType = check_operandType(rhsNode);

  // Resolve and load LHS value
  llvm::Value* lhsValue = nullptr;
  std::string lhsRegister;
  if (lhsType == "reg") {
    auto* lhsReg = dynamic_cast<RegisterNode*>(lhsNode);
    lhsRegister = lhsReg->registerName;
    lhsValue = getOrLoadRegister(lhsRegister);
  } else if (lhsType == "mem" || lhsType == "int") {
    lhsValue = castInputTypes(lhsNode, lhsType);
  } else {
    std::cerr << "Error: Unsupported LHS type for binary op\n";
    return;
  }

  // Resolve and load RHS value
  llvm::Value* rhsValue = castInputTypes(rhsNode, rhsType);
  if (!lhsValue || !rhsValue) {
    std::cerr << "Error: Failed to resolve operands\n";
    return;
  }

  // Promote pointers to int32 when needed
  if (lhsValue->getType()->isPointerTy()) {
    lhsValue = builder.CreatePtrToInt(
        lhsValue, llvm::Type::getInt32Ty(context), "lhs_int");
  }
  if (rhsValue->getType()->isPointerTy()) {
    rhsValue = builder.CreatePtrToInt(
        rhsValue, llvm::Type::getInt32Ty(context), "rhs_int");
  }

  // Lookup binary operation function
  auto it = binOpTable.find(node->opcode);
  if (it == binOpTable.end()) {
    std::cerr << "Error: Unsupported binary opcode '" << node->opcode << "'\n";
    return;
  }

  llvm::Value* result = it->second(builder, lhsValue, rhsValue);

  // Store result back into LHS register
  if (lhsType == "reg") {
    llvm::Value* destPtr = getNamedValue(lhsRegister);
    if (!destPtr || !destPtr->getType()->isPointerTy()) {
      destPtr = builder.CreateAlloca(
          llvm::Type::getInt32Ty(context), nullptr, lhsRegister);
      namedValues[lhsRegister] = destPtr;
    }
    builder.CreateStore(result, destPtr);
  } else if (lhsType == "mem") {
    auto* lhsMem = dynamic_cast<MemoryNode*>(lhsNode);
    llvm::Value* ptr = getOrLoadMemory(lhsMem);
    builder.CreateStore(result, ptr);
  } else {
    std::cerr << "Error: LHS must be assignable (register or memory)\n";
  }
}

void LLVMIRGen::handleMovInstructionNode(InstructionNode* node) {
  ASTNode* destNode = nullptr;
  ASTNode* srcNode = nullptr;

  if (node->operands.size() == 2) {
    destNode = node->operands[0].get();
    srcNode = node->operands[1].get();
  } else if (node->operands.size() == 3) {
    destNode = node->operands[1].get();
    srcNode = node->operands[2].get();
  } else if (node->operands.size() == 4) {
    destNode = node->operands[0].get();
    srcNode = node->operands[1].get();
  } else {
    std::cerr << "Invalid 'mov' operands: " << node->operands.size() << "\n";
    return;
  }

  if (!destNode || !srcNode) {
    std::cerr << "Error: 'mov' instruction requires two operands\n";
    return;
  }

  llvm::Function* function = builder.GetInsertBlock()->getParent();
  if (!function) {
    std::cerr << "Error: No parent function for 'mov' instruction\n";
    return;
  }

  std::string destType = check_operandType(destNode);
  std::string srcType = check_operandType(srcNode);

  llvm::Value* srcValue = nullptr;
  llvm::Value* srcPtr = nullptr;

  if (srcType == "int") {
    auto* intLit = dynamic_cast<IntLiteralNode*>(srcNode);
    srcValue =
        llvm::ConstantInt::get(context, llvm::APInt(32, intLit->value, true));
  } else if (srcType == "reg") {
    auto* srcReg = dynamic_cast<RegisterNode*>(srcNode);
    srcValue = getOrLoadRegister(srcReg->registerName);
  } else if (srcType == "mem") {
    auto* srcMem = dynamic_cast<MemoryNode*>(srcNode);
    llvm::Value* ptr = getOrLoadMemory(srcMem);
    srcValue =
        builder.CreateLoad(llvm::Type::getInt32Ty(context), ptr, "mem_load");
  } else {
    std::cerr << "Error: Invalid source operand for 'mov' instruction\n";
    return;
  }

  // store the value in a register
  if (destType == "reg") {
    auto* destReg = dynamic_cast<RegisterNode*>(destNode);
    llvm::Value* destPtr = getNamedValue(destReg->registerName);
    if (!destPtr || !destPtr->getType()->isPointerTy()) {
      destPtr = builder.CreateAlloca(
          llvm::Type::getInt32Ty(context), nullptr, destReg->registerName);
      namedValues[destReg->registerName] = destPtr;
    }
    builder.CreateStore(srcValue, destPtr);
  } else if (destType == "mem") {
    auto* destMem = dynamic_cast<MemoryNode*>(destNode);
    llvm::Value* ptr = getOrLoadMemory(destMem);
    builder.CreateStore(srcValue, ptr);
  } else {
    std::cerr << "Error: Invalid destination type for 'mov': " << destType
              << "\n";
    return;
  }
}

void LLVMIRGen::handleLeaInstructionNode(InstructionNode* node) {
  if (node->operands.size() != 2) {
    std::cerr << "Error: 'lea' requires exactly 2 operands\n";
    return;
  }

  ASTNode* destNode = node->operands[0].get();
  ASTNode* srcNode = node->operands[1].get();

  if (check_operandType(destNode) != "reg") {
    std::cerr << "Error: Destination of 'lea' must be a register\n";
    return;
  }

  auto* destReg = dynamic_cast<RegisterNode*>(destNode);
  llvm::Value* effectiveAddr = nullptr;

  if (check_operandType(srcNode) == "mem") {
    auto* memNode = dynamic_cast<MemoryNode*>(srcNode);

    // Handle global symbol like `msg`
    llvm::GlobalVariable* globalVar = module.getNamedGlobal(memNode->base);
    if (globalVar) {
      effectiveAddr =
          builder.CreateBitCast(globalVar, llvm::PointerType::get(context, 0));
      // We need to store this address in the destination register
      // But we need to ensure the destination register is allocated
      // means exists in namedValues
      if (!getNamedValue(destReg->registerName)) {
        // create an alloca for the destination reg if it doesn't exist
        llvm::Type* registerType = llvm::PointerType::get(context, 0);
        llvm::AllocaInst* allocaInst =
            builder.CreateAlloca(registerType, nullptr, destReg->registerName);
        namedValues[destReg->registerName] = allocaInst;
      }
      // store the effective address in the destination register
      llvm::Value* destPtr = getNamedValue(destReg->registerName);
      builder.CreateStore(effectiveAddr, destPtr);
    } else {
      std::cerr << "Error: Global variable '" << memNode->base
                << "' not found\n";
      return;
    }
  } else {
    std::cerr << "Error: Source of 'lea' must be a memory operand\n";
    return;
  }
}

void LLVMIRGen::handleIntInstructionNode(InstructionNode* node) {
  if (node->operands.size() != 1) {
    std::cerr << "Error: Invalid operands for 'int' instruction\n";
    return;
  }

  ASTNode* intNumNode = node->operands[0].get();
  auto* intLit = dynamic_cast<IntLiteralNode*>(intNumNode);
  if (!intLit) {
    std::cerr << "Error: Expected integer literal for 'int' instruction\n";
    return;
  }

  int interruptNumber = intLit->value;

  if (interruptNumber == 0x80) {
    // Get syscall number from EAX — registers are stored as i32 allocas,
    // so load the i32 value and zero-extend to i64 to match the syscall ABI.
    llvm::Value* eaxPtr = getNamedValue("eax");
    if (!eaxPtr) {
      std::cerr << "Error: EAX not set before 'int 0x80'\n";
      return;
    }
    llvm::Value* syscallNumber;
    if (eaxPtr->getType()->isPointerTy()) {
      llvm::Value* eaxVal = builder.CreateLoad(
          llvm::Type::getInt32Ty(context), eaxPtr, "eax_val");
      syscallNumber = builder.CreateZExt(
          eaxVal, llvm::Type::getInt64Ty(context), "syscall_num");
    } else {
      syscallNumber = builder.CreateZExtOrTrunc(
          eaxPtr, llvm::Type::getInt64Ty(context), "syscall_num");
    }
    llvm::Function* currentfunction = builder.GetInsertBlock()->getParent();
#ifdef __x86_64__
    // Handle x86-64 Linux system call via `int 0x80`
    llvm::Function* syscallFunc = module.getFunction("syscall");
    if (!syscallFunc) {
      llvm::FunctionType* syscallType = llvm::FunctionType::get(
          llvm::Type::getInt64Ty(context), // return type
          {
              llvm::Type::getInt64Ty(context), // syscall number
              llvm::Type::getInt64Ty(context), // arg1
              llvm::Type::getInt64Ty(context), // arg2
              llvm::Type::getInt64Ty(context), // arg3
              llvm::Type::getInt64Ty(context), // arg4
              llvm::Type::getInt64Ty(context), // arg5
              llvm::Type::getInt64Ty(context) // arg6
          },
          false);

      syscallFunc = llvm::Function::Create(
          syscallType, llvm::Function::ExternalLinkage, "syscall", module);
    }

    // Helper function to get register value (as i64) or zero
    auto getRegValue = [&](const std::string& reg) -> llvm::Value* {
      if (namedValues.count(reg)) {
        llvm::Value* regVal = namedValues[reg];
        // Registers are stored as i32 allocas; load i32 then zero-extend to i64
        if (regVal->getType()->isPointerTy()) {
          llvm::Value* loaded = builder.CreateLoad(
              llvm::Type::getInt32Ty(context), regVal, reg + "_i32");
          return builder.CreateZExt(
              loaded, llvm::Type::getInt64Ty(context), reg + "_i64");
        }
        return builder.CreateZExtOrTrunc(
            regVal, llvm::Type::getInt64Ty(context), reg + "_i64");
      }
      return llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    };

    // Get all arguments
    llvm::Value* args[6] = {
        getRegValue("ebx"),
        getRegValue("ecx"),
        getRegValue("edx"),
        getRegValue("esi"),
        getRegValue("edi"),
        getRegValue("ebp")};

    // Call syscall function
    llvm::Value* result = builder.CreateCall(
        syscallFunc,
        {syscallNumber, args[0], args[1], args[2], args[3], args[4], args[5]},
        "syscall_result");

    // Store result in EAX (as per x86 convention)
    // First ensure we have a pointer to EAX
    if (!namedValues.count("eax_ptr")) {
      // Create alloca for EAX if it doesn't exist
      llvm::IRBuilder<> entryBuilder(
          &currentfunction->getEntryBlock(),
          currentfunction->getEntryBlock().begin());
      llvm::Value* eaxAlloca = entryBuilder.CreateAlloca(
          llvm::Type::getInt64Ty(context), nullptr, "eax_ptr");
      namedValues["eax_ptr"] = eaxAlloca;
    }

    // Store the result
    builder.CreateStore(result, namedValues["eax_ptr"]);
    namedValues["eax"] = result;

#elif (defined(__linux__) || defined(__APPLE__)) && defined(__aarch64__)
    // ARM64 implementation
    auto getRegValue = [&](const std::string& reg) -> llvm::Value* {
      if (namedValues.count(reg)) {
        llvm::Value* regVal = namedValues[reg];
        if (regVal->getType()->isPointerTy()) {
          return builder.CreateLoad(llvm::Type::getInt64Ty(context), regVal);
        }
        return builder.CreateZExtOrTrunc(
            regVal, llvm::Type::getInt64Ty(context));
      }
      return llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    };

    // Get syscall number and check if it's constant
    llvm::Value* syscallNum = getRegValue("eax");
    bool isExitSyscall = false;

    if (auto* constInt = llvm::dyn_cast<llvm::ConstantInt>(syscallNum)) {
      uint64_t syscallNumVal = constInt->getZExtValue();

      // Handle specific syscalls
      if (syscallNumVal == 1) { // exit
        llvm::Value* exitStatus = getRegValue("ebx");
#if defined(__linux__) && defined(__aarch64__)
        constexpr int SYSCALL_EXIT = 93;
        SyscallBuilder::emitExitSyscall(
            builder, context, &module, exitStatus, TargetABI::Linux_ARM64);
#elif defined(__APPLE__) && defined(__aarch64__)
        constexpr int SYSCALL_EXIT = 0x2000001;
        SyscallBuilder::emitExitSyscall(
            builder, context, &module, exitStatus, TargetABI::MacOS_ARM64);
#endif
        isExitSyscall = true;
      } else if (syscallNumVal == 4) { // write
        llvm::Value* fd = getRegValue("ebx");
        llvm::Value* buf = getRegValue("ecx");
        llvm::Value* len = getRegValue("edx");

        std::vector<llvm::Value*> writeArgs = {fd, buf, len};

#if defined(__linux__) && defined(__aarch64__)
        constexpr int SYSCALL_WRITE = 64;
        llvm::Value* result = SyscallBuilder::emitSyscall(
            builder,
            context,
            &module,
            writeArgs,
            SYSCALL_WRITE,
            TargetABI::Linux_ARM64);
#elif defined(__APPLE__) && defined(__aarch64__)
        constexpr int SYSCALL_WRITE = 0x2000004;
        llvm::Value* result = SyscallBuilder::emitSyscall(
            builder,
            context,
            &module,
            writeArgs,
            SYSCALL_WRITE,
            TargetABI::MacOS_ARM64);
#endif
        // Store result in EAX
        if (!namedValues.count("eax_ptr")) {
          llvm::IRBuilder<> entryBuilder(
              &currentfunction->getEntryBlock(),
              currentfunction->getEntryBlock().begin());
          llvm::Value* eaxAlloca = entryBuilder.CreateAlloca(
              llvm::Type::getInt64Ty(context), nullptr, "eax_ptr");
          namedValues["eax_ptr"] = eaxAlloca;
        }
        builder.CreateStore(result, namedValues["eax_ptr"]);
        namedValues["eax"] = result;
      }
    }

    if (!isExitSyscall) {
      // Generic syscall handling for non-exit cases
      llvm::Value* args[6] = {
          getRegValue("ebx"),
          getRegValue("ecx"),
          getRegValue("edx"),
          getRegValue("esi"),
          getRegValue("edi"),
          getRegValue("ebp")};

#if defined(__linux__) && defined(__aarch64__)
      llvm::Value* result = SyscallBuilder::emitGenericSyscall(
          builder, context, &module, syscallNum, args, TargetABI::Linux_ARM64);
#elif defined(__APPLE__) && defined(__aarch64__)
      llvm::Value* result = SyscallBuilder::emitGenericSyscall(
          builder, context, &module, syscallNum, args, TargetABI::MacOS_ARM64);
#endif

      // Store result in EAX
      if (result) {
        if (!namedValues.count("eax_ptr")) {
          llvm::IRBuilder<> entryBuilder(
              &currentfunction->getEntryBlock(),
              currentfunction->getEntryBlock().begin());
          llvm::Value* eaxAlloca = entryBuilder.CreateAlloca(
              llvm::Type::getInt64Ty(context), nullptr, "eax_ptr");
          namedValues["eax_ptr"] = eaxAlloca;
        }
        builder.CreateStore(result, namedValues["eax_ptr"]);
        namedValues["eax"] = result;
      }
    }

#else
    std::cerr << "Error: Unsupported architecture\n";
#endif
  } else {
    std::cerr << "Error: Unsupported interrupt number: " << interruptNumber
              << "\n";
  }
}

void LLVMIRGen::handleSyscallInstructionNode(InstructionNode* node) {}

void LLVMIRGen::handleRetInstructionNode(InstructionNode* node) {
  if (node->operands.empty()) {
    builder.CreateRetVoid();
  } else {
    if (node->operands.size() != 1) {
      std::cerr << "Error: 'ret' instruction requires exactly one operand\n";
      return;
    }
    auto* returnTarget = dynamic_cast<MemoryNode*>(node->operands[0].get());
    if (!returnTarget) {
      std::cerr << "Error: Invalid return type for 'ret' instruction\n";
      return;
    }
    llvm::Value* retVal = nullptr;
    if (returnTarget->base == "eax") {
      retVal = getNamedValue("eax");
    } else if (returnTarget->base == "rax") {
      retVal = getNamedValue("rax");
    } else {
      std::cerr << "Error: Unsupported return target: " << returnTarget->base
                << "\n";
      return;
    }
    if (!retVal) {
      std::cerr << "Error: Return value not found\n";
      return;
    }
    // Create a return instruction
    llvm::Function* function = builder.GetInsertBlock()->getParent();
    if (!function) {
      std::cerr << "Error: No parent function for 'ret' instruction\n";
      return;
    }
    llvm::BasicBlock* exitBlock =
        llvm::BasicBlock::Create(context, "exit", function);
    builder.CreateBr(exitBlock);
    builder.SetInsertPoint(exitBlock);
    builder.CreateRet(retVal);
  }
}

void LLVMIRGen::handleCompareInstructionNode(InstructionNode* node) {
  if (node->operands.size() != 2) {
    std::cerr
        << "Error: The Compare Instruction requires exactly two operands.\n";
    return;
  }
  ASTNode* lhsNode = node->operands[0].get();
  ASTNode* rhsNode = node->operands[1].get();

  // destination register for comparison it should also be a memory
  std::string lhsType = check_operandType(lhsNode);
  std::string rhsType = check_operandType(rhsNode);

  if (lhsType != "reg" && lhsType != "mem" && lhsType != "int") {
    std::cerr << "Error: Left operand must be a supported type.\n";
    return;
  }
  if (rhsType != "reg" && rhsType != "int" && rhsType != "mem") {
    std::cerr << "Error: Right operand must be a supported type.\n";
    return;
  }
  llvm::Value* lhsValue = castInputTypes(lhsNode, lhsType);
  llvm::Value* rhsValue = castInputTypes(rhsNode, rhsType);
  if (!lhsValue || !rhsValue) {
    std::cerr << "Error: Invalid operands for comparison\n";
    return;
  }
  if (lhsValue->getType()->isPointerTy())
    lhsValue =
        builder.CreatePtrToInt(lhsValue, llvm::Type::getInt32Ty(context));

  if (rhsValue->getType()->isPointerTy())
    rhsValue =
        builder.CreatePtrToInt(rhsValue, llvm::Type::getInt32Ty(context));

  if (node->opcode == "cmp") {
    llvm::Value* result = builder.CreateSub(lhsValue, rhsValue, "cmp_tmp");

    // Update Zero Flag (ZF)
    ContextCPUState.zeroFlag = builder.CreateICmpEQ(
        result, llvm::ConstantInt::get(result->getType(), 0), "zeroFlag");

    // Update Sign Flag (SF)
    ContextCPUState.signFlag = builder.CreateICmpSLT(
        result, llvm::ConstantInt::get(result->getType(), 0), "signFlag");

    // Update Carry Flag (CF)
    ContextCPUState.carryFlag =
        builder.CreateICmpULT(lhsValue, rhsValue, "carryFlag");

    // Update Overflow Flag (OF)
    llvm::Value* lhsSign = builder.CreateICmpSLT(
        lhsValue, llvm::ConstantInt::get(lhsValue->getType(), 0));
    llvm::Value* rhsSign = builder.CreateICmpSLT(
        rhsValue, llvm::ConstantInt::get(rhsValue->getType(), 0));
    llvm::Value* resultSign = builder.CreateICmpSLT(
        result, llvm::ConstantInt::get(result->getType(), 0));

    ContextCPUState.overflowFlag = builder.CreateXor(
        builder.CreateXor(lhsSign, rhsSign), resultSign, "overflowFlag");
  } else if (node->opcode == "test") {
    llvm::Value* result = builder.CreateAnd(lhsValue, rhsValue, "test_tmp");

    // Update Zero Flag (ZF): Set if result is zero
    ContextCPUState.zeroFlag = builder.CreateICmpEQ(
        result, llvm::ConstantInt::get(result->getType(), 0), "zeroFlag");

    // Update Sign Flag (SF): Set if result is negative
    ContextCPUState.signFlag = builder.CreateICmpSLT(
        result, llvm::ConstantInt::get(result->getType(), 0), "signFlag");

    // Clear Carry Flag (CF)
    ContextCPUState.carryFlag = llvm::ConstantInt::getFalse(context);

    // Clear Overflow Flag (OF)
    ContextCPUState.overflowFlag = llvm::ConstantInt::getFalse(context);

  } else {
    std::cerr << "This comparison instruction is not supported yet!!: "
              << node->opcode << "\n";
    return;
  }
}

void LLVMIRGen::handleBranchingInstructions(InstructionNode* node) {
  if (node->operands.size() != 1) {
    std::cerr << "Error: Missing branch target for '" << node->opcode << "'\n";
    return;
  }

  auto* labelNode = dynamic_cast<MemoryNode*>(node->operands[0].get());
  if (!labelNode) {
    std::cerr << "Error: Invalid label type for branch\n";
    return;
  }

  std::string targetLabel = entryBlockNames[labelNode->base];
  if (labelMap.find(targetLabel) == labelMap.end()) {
    std::cerr << "Error: Branch target '" << targetLabel << "' not found\n";
    return;
  }

  llvm::BasicBlock* trueBlock = labelMap[targetLabel];
  llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();

  // Handle unconditional jump first
  if (node->opcode == "jmp") {
    builder.CreateBr(trueBlock);
    builder.SetInsertPoint(trueBlock);
    return;
  }

  // Conditional
  llvm::Value* cond = nullptr;

  if (node->opcode == "je" || node->opcode == "jz") {
    cond = ContextCPUState.zeroFlag;
  } else if (node->opcode == "jne" || node->opcode == "jnz") {
    cond = builder.CreateNot(ContextCPUState.zeroFlag, "not_zf");
  } else if (node->opcode == "jg") {
    // (ZF == 0) && (SF == OF)
    llvm::Value* zf0 = builder.CreateNot(ContextCPUState.zeroFlag, "not_zf");
    llvm::Value* s_eq_o = builder.CreateICmpEQ(
        ContextCPUState.signFlag, ContextCPUState.overflowFlag, "sf_eq_of");
    cond = builder.CreateAnd(zf0, s_eq_o, "jg_cond");
  } else if (node->opcode == "jge") {
    cond = builder.CreateICmpEQ(
        ContextCPUState.signFlag, ContextCPUState.overflowFlag, "jge_cond");
  } else if (node->opcode == "jl") {
    cond = builder.CreateICmpNE(
        ContextCPUState.signFlag, ContextCPUState.overflowFlag, "jl_cond");
  } else if (node->opcode == "jle") {
    llvm::Value* s_ne_o = builder.CreateICmpNE(
        ContextCPUState.signFlag, ContextCPUState.overflowFlag);
    cond = builder.CreateOr(s_ne_o, ContextCPUState.zeroFlag, "jle_cond");
  } else if (node->opcode == "jc") {
    cond = ContextCPUState.carryFlag;
  } else if (node->opcode == "jnc") {
    cond = builder.CreateNot(ContextCPUState.carryFlag, "not_cf");
  } else {
    std::cerr << "Error: Unsupported branch opcode: " << node->opcode << "\n";
    return;
  }

  // Fallback: create a dummy false block if not terminated
  llvm::BasicBlock* falseBlock =
      llvm::BasicBlock::Create(context, "branch_fallback", currentFunc);
  builder.CreateCondBr(cond, trueBlock, falseBlock);
  builder.SetInsertPoint(falseBlock);
}

void LLVMIRGen::handleCallInstructionNode(InstructionNode* node) {
  llvm::Function* parentFunction = builder.GetInsertBlock()->getParent();
  if (!parentFunction) {
    std::cerr << "Error: No parent function for call instruction\n";
    return;
  }

  if (node->operands.size() < 1) {
    std::cerr << "Error: Call instruction missing callee operand\n";
    return;
  }

  auto* calleeNode = dynamic_cast<MemoryNode*>(node->operands[0].get());
  if (!calleeNode) {
    std::cerr << "Error: Invalid callee for call instruction\n";
    return;
  }

  std::string functionName = calleeNode->base;
  std::vector<llvm::Value*> args;
  for (size_t i = 1; i < node->operands.size(); ++i) {
    llvm::Value* arg = castInputTypes(
        node->operands[i].get(), check_operandType(node->operands[i].get()));
    if (!arg) {
      std::cerr << "Error: Failed to evaluate argument " << i << "\n";
      return;
    }
    args.push_back(arg);
  }

  llvm::Function* calleeFunction = nullptr;
  if (definedFunctionsMap.count(functionName)) {
    calleeFunction = definedFunctionsMap[functionName];
  } else {
    std::vector<llvm::Type*> argTypes;
    for (auto* arg : args) {
      argTypes.push_back(arg->getType());
    }

    llvm::FunctionType* funcType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context), argTypes, false);

    calleeFunction = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, functionName, module);
    definedFunctionsMap[functionName] = calleeFunction;
  }

  // Snapshot caller-saved registers and stack
  snapshotRegisterState(parentFunction);
  snapshotStackState(parentFunction);

  // Emit function call
  llvm::CallInst* call = builder.CreateCall(calleeFunction, args);
  call->setTailCall(false);

  if (calleeFunction && calleeFunction->empty()) {
    std::unordered_map<std::string, llvm::Value*> snapshot;
    for (const auto& reg : {"eax", "ebx", "ecx", "edx", "esi", "edi"}) {
      std::string shadowName = std::string(reg) + "_shadow";
      if (namedValues.count(shadowName)) {
        snapshot[reg] =
            namedValues[shadowName]; // map "eax" → "eax_shadow" pointer
      }
    }

    // Stack pointers (also treated as shadow values)
    if (namedValues.count("rsp_shadow")) {
      snapshot["rsp"] = namedValues["rsp_shadow"];
    }
    if (namedValues.count("rsp_ptr_shadow")) {
      snapshot["rsp_ptr"] = namedValues["rsp_ptr_shadow"];
    }

    calleeContext[calleeFunction] = snapshot;
  }

  // Restore caller-saved registers and stack
  restoreStackState(parentFunction);
  restoreRegisterState(parentFunction);
  llvm::Value* retVal = getOrLoadRegister("eax");
}

void LLVMIRGen::handleDivInstructionNode(InstructionNode* node) {
  if (node->operands.size() != 1) {
    std::cerr << "Error: 'div' expects one operand (the divisor)\n";
    return;
  }

  llvm::Value* divisor = castInputTypes(
      node->operands[0].get(), check_operandType(node->operands[0].get()));
  if (!divisor)
    return;

  if (divisor->getType()->isPointerTy())
    divisor = builder.CreatePtrToInt(
        divisor, llvm::Type::getInt32Ty(context), "divisor_int");

  // Load EAX and EDX
  llvm::Value* eaxVal = getOrLoadRegister("eax");
  llvm::Value* edxVal = getOrLoadRegister("edx");

  // Extend to 64-bit
  llvm::Value* eaxExt =
      builder.CreateZExt(eaxVal, llvm::Type::getInt64Ty(context), "zext_eax");
  llvm::Value* edxExt =
      builder.CreateZExt(edxVal, llvm::Type::getInt64Ty(context), "zext_edx");

  llvm::Value* dividend = builder.CreateOr(
      builder.CreateShl(
          edxExt, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 32)),
      eaxExt,
      "dividend");

  llvm::Value* divisor64 = builder.CreateZExt(
      divisor, llvm::Type::getInt64Ty(context), "divisor_zext");

  // Zero-check
  llvm::Value* isZero = builder.CreateICmpEQ(
      divisor64, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0));
  llvm::Function* func = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock* divOk = llvm::BasicBlock::Create(context, "div.ok", func);
  llvm::BasicBlock* divErr = llvm::BasicBlock::Create(context, "div.err", func);

  builder.CreateCondBr(isZero, divErr, divOk);

  builder.SetInsertPoint(divErr);
  builder.CreateCall(
      getPrintFunction(),
      {builder.CreateGlobalStringPtr("Divide by zero error\n")});
  builder.CreateUnreachable();

  builder.SetInsertPoint(divOk);
  llvm::Value* quotient = builder.CreateUDiv(dividend, divisor64, "quotient");
  llvm::Value* remainder = builder.CreateURem(dividend, divisor64, "remainder");

  // Truncate and store into registers
  llvm::Value* q32 =
      builder.CreateTrunc(quotient, llvm::Type::getInt32Ty(context), "quot32");
  llvm::Value* r32 =
      builder.CreateTrunc(remainder, llvm::Type::getInt32Ty(context), "rem32");

  llvm::Value* eaxPtr = getNamedValue("eax");
  llvm::Value* edxPtr = getNamedValue("edx");
  builder.CreateStore(q32, eaxPtr);
  builder.CreateStore(r32, edxPtr);
}

void LLVMIRGen::handleLoopInstructionNode(InstructionNode* node) {}

void LLVMIRGen::initializeFunctionStack(llvm::Function* func) {
  if (rspIntPerFunction.count(func))
    return;

  llvm::IRBuilder<> tmpBuilder(
      &func->getEntryBlock(), func->getEntryBlock().begin());
  auto* stackArrayType =
      llvm::ArrayType::get(llvm::Type::getInt32Ty(context), 1024);
  auto* stackMem = tmpBuilder.CreateAlloca(
      stackArrayType, nullptr, "stackmem_" + func->getName().str());
  auto* rsp = tmpBuilder.CreateAlloca(
      llvm::Type::getInt32Ty(context), nullptr, "rsp_" + func->getName().str());
  tmpBuilder.CreateStore(
      llvm::ConstantInt::get(context, llvm::APInt(32, 1024)), rsp);
  // pointer stack
  auto* ptrStackArrayType = llvm::ArrayType::get(
      llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0), 1024);
  auto* ptrStack = tmpBuilder.CreateAlloca(
      ptrStackArrayType, nullptr, "stackmem_ptr_" + func->getName().str());
  auto* rspPtr = tmpBuilder.CreateAlloca(
      llvm::Type::getInt32Ty(context),
      nullptr,
      "rsp_ptr_" + func->getName().str());
  tmpBuilder.CreateStore(
      llvm::ConstantInt::get(context, llvm::APInt(32, 0)), rspPtr);

  stackMemIntPerFunction[func] = stackMem;
  rspIntPerFunction[func] = rsp;
  stackMemPtrPerFunction[func] = ptrStack;
  rspPtrPerFunction[func] = rspPtr;
}

void LLVMIRGen::handleStackOperationInstructionNode(InstructionNode* node) {
  llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();

  llvm::Value* rsp = getNamedValue("rsp_shadow");
  llvm::Value* rspPtr = getNamedValue("rsp_ptr_shadow");

  if (!rsp || !rspPtr) {
    std::cerr
        << "Error: No stack snapshot (rsp_shadow) found in callee context\n";
    return;
  }

  llvm::Function* callerFunc = nullptr;
  for (const auto& [func, ctx] : calleeContext) {
    if (func == currentFunc) {
      callerFunc = builder.GetInsertBlock()->getParent();
      break;
    }
  }

  if (!callerFunc || !stackMemIntPerFunction.count(callerFunc)) {
    std::cerr << "Error: No stack memory snapshot found for callee\n";
    return;
  }

  auto& stackInt = stackMemIntPerFunction[callerFunc];
  auto& stackPtr = stackMemPtrPerFunction[callerFunc];

  const std::string& opcode = node->opcode;

  if (opcode == "push") {
    ASTNode* valNode = node->operands[0].get();
    std::string valType = check_operandType(valNode);
    llvm::Value* val = nullptr;
    if (valType != "reg" && valType != "mem" && valType != "int") {
      std::cerr << "Error: Invalid value type for 'push': " << valType << "\n";
      return;
    }
    val = castInputTypes(valNode, valType);

    llvm::Value* rspVal =
        builder.CreateLoad(llvm::Type::getInt32Ty(context), rsp);
    llvm::Value* newRsp = builder.CreateSub(
        rspVal, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
    builder.CreateStore(newRsp, rsp);

    llvm::Value* ptr = builder.CreateInBoundsGEP(
        llvm::ArrayType::get(llvm::Type::getInt32Ty(context), 1024),
        stackInt,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0), newRsp},
        "stack_push_ptr");

    builder.CreateStore(val, ptr);

  } else if (opcode == "pop") {
    ASTNode* destNode = node->operands[0].get();
    std::string destType = check_operandType(destNode);

    llvm::Value* rspVal =
        builder.CreateLoad(llvm::Type::getInt32Ty(context), rsp);

    llvm::Value* ptr = builder.CreateInBoundsGEP(
        llvm::ArrayType::get(llvm::Type::getInt32Ty(context), 1024),
        stackInt,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0), rspVal},
        "stack_pop_ptr");

    llvm::Value* val = builder.CreateLoad(llvm::Type::getInt32Ty(context), ptr);

    if (destType == "reg") {
      auto* regNode = dynamic_cast<RegisterNode*>(destNode);
      llvm::Value* regPtr = getOrLoadRegister(regNode->registerName);
      builder.CreateStore(val, regPtr);
    } else if (destType == "mem") {
      auto* memNode = dynamic_cast<MemoryNode*>(destNode);
      llvm::Value* destPtr = getOrLoadMemory(memNode);
      builder.CreateStore(val, destPtr);
    }

    llvm::Value* newRsp = builder.CreateAdd(
        rspVal, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
    builder.CreateStore(newRsp, rsp);

  } else {
    std::cerr << "Unsupported stack opcode: " << opcode << "\n";
  }
}

void LLVMIRGen::handleUnaryInstructionNode(
    InstructionNode* node,
    const std::string& opType) {
  if (node->operands.size() != 1) {
    std::cerr << "Error: Unary instruction requires exactly one operand\n";
    return;
  }

  ASTNode* operandNode = node->operands[0].get();
  std::string operandType = check_operandType(operandNode);
  llvm::Value* operandPtr = nullptr;
  llvm::Value* value = nullptr;

  if (operandType == "reg") {
    auto* regNode = dynamic_cast<RegisterNode*>(operandNode);
    operandPtr = getNamedValue(regNode->registerName);

    if (!operandPtr || !operandPtr->getType()->isPointerTy()) {
      operandPtr = builder.CreateAlloca(
          llvm::Type::getInt32Ty(context), nullptr, regNode->registerName);
      namedValues[regNode->registerName] = operandPtr;
    }

    value = builder.CreateLoad(
        llvm::Type::getInt32Ty(context),
        operandPtr,
        regNode->registerName + "_load");

  } else if (operandType == "mem") {
    auto* memNode = dynamic_cast<MemoryNode*>(operandNode);
    operandPtr = getOrLoadMemory(memNode);
    value = builder.CreateLoad(
        llvm::Type::getInt32Ty(context), operandPtr, "mem_load");

  } else {
    std::cerr << "Error: Unsupported operand type for unary instruction: "
              << operandType << "\n";
    return;
  }

  llvm::Value* result = nullptr;
  if (opType == "inc") {
    result = builder.CreateAdd(
        value, llvm::ConstantInt::get(context, llvm::APInt(32, 1)), "inc");
  } else if (opType == "dec") {
    result = builder.CreateSub(
        value, llvm::ConstantInt::get(context, llvm::APInt(32, 1)), "dec");
  } else if (opType == "neg") {
    result = builder.CreateNeg(value, "neg");
  } else {
    std::cerr << "Error: Unknown unary operation: " << opType << "\n";
    return;
  }

  builder.CreateStore(result, operandPtr);
}
