#include "llvm_ir/LLVMIRGenerator.h"

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

// syscall builer class
void SyscallBuilder::emitSyscall(
    llvm::IRBuilder<>& builder,
    llvm::LLVMContext& context,
    llvm::Module* module,
    const std::vector<llvm::Value*>& args,
    uint64_t syscallNumber,
    TargetABI targetABI) {
  assert(args.size() <= 6 && "Too many syscall arguments");

  llvm::FunctionType* funcType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),
      std::vector<llvm::Type*>(args.size(), llvm::Type::getInt64Ty(context)),
      false);

  std::string asmString;
  std::string constraints;

  if (targetABI == TargetABI::MacOS_ARM64) {
    asmString = generateMacOSInlineAsm(args.size(), syscallNumber);
    constraints = generateConstraints(args.size());
  } else if (targetABI == TargetABI::Linux_ARM64) {
    asmString = generateLinuxARM64InlineAsm(args.size(), syscallNumber);
    constraints = generateConstraints(args.size());
  } else {
    // Add more ABIs or error
    return;
  }

  llvm::InlineAsm* inlineAsm =
      llvm::InlineAsm::get(funcType, asmString, constraints, true);
  builder.CreateCall(inlineAsm, args);
}

void SyscallBuilder::emitExitSyscall(
    llvm::IRBuilder<>& builder,
    llvm::LLVMContext& context,
    llvm::Module* module,
    llvm::Value* status,
    TargetABI targetABI) {
  emitSyscall(
      builder, context, module, {status}, 1, targetABI); // syscall 1 = exit
}

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
  // set the named values for this context used by stack
  std::unordered_map<std::string, llvm::Value*> copiedNamedValues;

  for (auto& arg : namedValues) {
    std::string reg = arg.first;
    if (reg == "eax" || reg == "ebx" || reg == "ecx" || reg == "edx" ||
        reg == "esi" || reg == "edi") {
      if (!arg.second)
        continue;
      copiedNamedValues[reg] = arg.second;
    }
  }

  namedValues.clear();
  namedValues = copiedNamedValues;

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
    std::cerr << "Error: Binary operation: " << node->opcode
              << " requires two operands.\n";
    return;
  }
  if (!lhsNode || !rhsNode) {
    std::cerr << "Error: Binary operation requires two operands.\n";
    return;
  }
  // Get the destination register
  auto* lhsReg = dynamic_cast<RegisterNode*>(lhsNode);
  if (!lhsReg) {
    std::cerr << "Error: Left operand must be a register.\n";
    return;
  }

  llvm::Value* lhs = getNamedValue(lhsReg->registerName);
  if (!lhs) {
    // Check if the register is initialized
    // some system calls like xor edi, edi don't
    // initialize the register
    std::cerr << "Error: Register " << lhsReg->registerName
              << " is not initialized. " << "Initializing to 0.\n";
    lhs = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));
    llvm::Value* allocaInst = builder.CreateAlloca(
        llvm::Type::getInt32Ty(context), nullptr, lhsReg->registerName);
    builder.CreateStore(lhs, allocaInst);
    namedValues[lhsReg->registerName] = allocaInst;
    lhs = builder.CreateLoad(
        llvm::Type::getInt32Ty(context),
        allocaInst,
        "load_" + lhsReg->registerName);
  }

  // Get the type of the left operand as it can be either a
  // register or memory location
  std::string lhsType = check_operandType(lhsNode);
  std::string rhsType = check_operandType(rhsNode);

  if (lhsType == "reg") {
    auto* lhsReg = dynamic_cast<RegisterNode*>(lhsNode);
    if (!lhsReg) {
      std::cerr << "Error: Left operand must be a register.\n";
      return;
    }

    llvm::Value* lhs = getNamedValue(lhsReg->registerName);
  } else if (lhsType == "mem") {
    auto* lhsMem = dynamic_cast<MemoryNode*>(lhsNode);
    if (!lhsMem) {
      std::cerr << "Error: Invalid memory operand\n";
      return;
    }
    llvm::Value* ptr = handleDestinationMemory(lhsMem);
    llvm::Value* lhs =
        builder.CreateLoad(llvm::Type::getInt32Ty(context), ptr, "mem_load");
  } else {
    std::cerr << "Error: Invalid left operand for binary operation.\n";
    return;
  }
  // Ensure lhs is integer
  if (lhs->getType()->isPointerTy()) {
    lhs =
        builder.CreatePtrToInt(lhs, llvm::Type::getInt32Ty(context), "lhs_int");
  }
  llvm::Value* rhs = castInputTypes(rhsNode, rhsType);

  if (rhs->getType()->isPointerTy()) {
    rhs =
        builder.CreatePtrToInt(rhs, llvm::Type::getInt32Ty(context), "rhs_int");
  }

  // Lookup the operation
  auto it = binOpTable.find(node->opcode);
  if (it == binOpTable.end()) {
    std::cerr << "Error: Unsupported binary opcode '" << node->opcode << "'\n";
    return;
  }
  // Generate the LLVM IR instruction
  llvm::Value* result = it->second(builder, lhs, rhs);

  // Store the result in the destination register
  namedValues[lhsReg->registerName] = result;
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
    llvm::Value* srcRegValue = getNamedValue(srcReg->registerName);
    if (!srcRegValue) {
      std::cerr << "Error: Source register " << srcReg->registerName
                << " not found " << "Allocating it.\n";
      srcPtr = builder.CreateAlloca(
          llvm::Type::getInt32Ty(context), nullptr, srcReg->registerName);
      namedValues[srcReg->registerName] = srcPtr;
      srcValue = builder.CreateLoad(
          llvm::Type::getInt32Ty(context),
          srcPtr,
          srcReg->registerName + "_load");
    } else {
      if (srcRegValue->getType()->isPointerTy()) {
        srcPtr = srcRegValue;
        srcValue = builder.CreateLoad(
            llvm::Type::getInt32Ty(context),
            srcPtr,
            srcReg->registerName + "_load");
      } else if (srcRegValue->getType()->isIntegerTy(32)) {
        srcPtr = builder.CreateIntToPtr(
            srcRegValue,
            llvm::PointerType::get(llvm::Type::getInt32Ty(context), 0),
            srcReg->registerName + "_ptrcast");
        srcValue = builder.CreateLoad(
            llvm::Type::getInt32Ty(context),
            srcPtr,
            srcReg->registerName + "_load");
      } else {
        std::cerr << "Error: Source register " << srcReg->registerName
                  << " is not of type int32\n";
        return;
      }
    }
  } else if (srcType == "mem") {
    auto* memNode = dynamic_cast<MemoryNode*>(srcNode);
    if (!memNode) {
      std::cerr << "Error: Invalid memory operand\n";
      return;
    }

    llvm::Value* baseAddr = nullptr;
    llvm::Value* offsetVal = nullptr;

    if (!memNode->base.empty()) {
      if (!getNamedValue(memNode->base)) {
        std::cerr << "Error: Base register " << memNode->base
                  << " not allocated yet. Allocating it.\n";
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
        std::cerr << "Error: Invalid memory offset '" << memNode->offset
                  << "'\n";
        return;
      }
    }

    llvm::Value* memAddr = baseAddr;
    if (offsetVal) {
      memAddr = builder.CreateAdd(baseAddr, offsetVal, "mem_addr");
    }

    llvm::Type* i32PtrType =
        llvm::PointerType::get(llvm::Type::getInt32Ty(context), 0);
    llvm::Value* ptr = builder.CreateIntToPtr(memAddr, i32PtrType, "ptr_cast");

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
    if (!destPtr) {
      destPtr = builder.CreateAlloca(
          llvm::Type::getInt32Ty(context), nullptr, destReg->registerName);
      namedValues[destReg->registerName] = destPtr;
    }
    // ensure that the destination is a pointer
    if (!destPtr->getType()->isPointerTy()) {
      destPtr = builder.CreateAlloca(
          llvm::Type::getInt32Ty(context), nullptr, destReg->registerName);
    }
    builder.CreateStore(srcValue, destPtr);
  } else if (destType == "mem") {
    auto* destMem = dynamic_cast<MemoryNode*>(destNode);
    if (!destMem) {
      std::cerr << "Error: Invalid memory operand\n";
      return;
    }
    llvm::Value* ptr = handleDestinationMemory(destMem);
    builder.CreateStore(srcValue, ptr);
  } else {
    std::cerr << "Error: Destination operand must be a register not of type: "
              << destType << "\n";
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
#ifdef __x86_64__
    // Handle x86-64 Linux system call via `int 0x80`
    llvm::Function* syscallFunc = module.getFunction("syscall");
    if (!syscallFunc) {
      llvm::FunctionType* syscallType = llvm::FunctionType::get(
          llvm::Type::getInt64Ty(context), // return type
          {llvm::Type::getInt64Ty(context), // syscall number
           llvm::Type::getInt64Ty(context),
           llvm::Type::getInt64Ty(context),
           llvm::Type::getInt64Ty(context),
           llvm::Type::getInt64Ty(context),
           llvm::Type::getInt64Ty(context),
           llvm::Type::getInt64Ty(context)}, // arguments
          false);

      syscallFunc = llvm::Function::Create(
          syscallType, llvm::Function::ExternalLinkage, "syscall", module);
    }

    // Get syscall number from EAX
    llvm::Value* syscallNumber = getNamedValue("eax");
    if (!syscallNumber) {
      std::cerr << "Error: EAX not set before 'int 0x80'\n";
      return;
    }

    // Get syscall arguments from registers (EBX, ECX, etc.)
    llvm::Value* arg1 = namedValues.count("ebx")
        ? namedValues["ebx"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    llvm::Value* arg2 = namedValues.count("ecx")
        ? namedValues["ecx"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    llvm::Value* arg3 = namedValues.count("edx")
        ? namedValues["edx"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    llvm::Value* arg4 = namedValues.count("esi")
        ? namedValues["esi"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    llvm::Value* arg5 = namedValues.count("edi")
        ? namedValues["edi"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    llvm::Value* arg6 = namedValues.count("ebp")
        ? namedValues["ebp"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));

    // Call syscall function
    llvm::Value* result = builder.CreateCall(
        syscallFunc,
        {syscallNumber, arg1, arg2, arg3, arg4, arg5, arg6},
        "syscall_result");

    // Store result in EAX (as per x86 convention)
    namedValues["eax"] = result;

#elif (defined(__linux__) || defined(__APPLE__)) && defined(__aarch64__)
    // Helper function to load and convert values to i64
    auto loadAndConvertToInt64 = [&](const std::string& regName,
                                     llvm::Type* regType) -> llvm::Value* {
      if (namedValues.count(regName)) {
        llvm::Value* regValue = getNamedValue(regName);
        if (regValue->getType()->isPointerTy()) {
          llvm::Value* loaded = builder.CreateLoad(regType, regValue);
          return builder.CreateZExtOrTrunc(
              loaded, llvm::Type::getInt64Ty(context));
        } else if (regValue->getType()->isIntegerTy()) {
          return builder.CreateZExtOrTrunc(
              regValue, llvm::Type::getInt64Ty(context));
        } else {
          throw std::runtime_error("Unsupported type for: " + regName);
        }
      } else {
        return llvm::ConstantInt::get(context, llvm::APInt(64, 0));
      }
    };
    // Common syscall args for write(fd, buf, len)
    llvm::Value* arg0 =
        loadAndConvertToInt64("ebx", llvm::Type::getInt32Ty(context));
    llvm::Value* arg2 =
        loadAndConvertToInt64("edx", llvm::Type::getInt32Ty(context));
    llvm::Value* arg1 = nullptr;

    if (namedValues.count("ecx")) {
      llvm::Value* loadedPtr = builder.CreateLoad(
          llvm::PointerType::get(context, 0), namedValues["ecx"]);
      arg1 = builder.CreatePtrToInt(loadedPtr, llvm::Type::getInt64Ty(context));
    } else {
      arg1 = llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    }

    std::vector<llvm::Value*> writeArgs = {arg0, arg1, arg2};

    llvm::Value* exitStatus =
        loadAndConvertToInt64("ebx", llvm::Type::getInt32Ty(context));

#if defined(__linux__) && defined(__aarch64__)
    constexpr int SYSCALL_WRITE = 64;
    constexpr int SYSCALL_EXIT = 93;

    SyscallBuilder::emitSyscall(
        builder,
        context,
        &module,
        writeArgs,
        SYSCALL_WRITE,
        TargetABI::Linux_ARM64);
    SyscallBuilder::emitExitSyscall(
        builder, context, &module, exitStatus, TargetABI::Linux_ARM64);

#elif defined(__APPLE__) && defined(__aarch64__)
    constexpr int SYSCALL_WRITE = 0x2000004;
    constexpr int SYSCALL_EXIT = 0x2000001;

    SyscallBuilder::emitSyscall(
        builder,
        context,
        &module,
        writeArgs,
        SYSCALL_WRITE,
        TargetABI::MacOS_ARM64);
    SyscallBuilder::emitExitSyscall(
        builder, context, &module, exitStatus, TargetABI::MacOS_ARM64);
#endif

#else
    std::cerr << "Error: Unsupported architecture\n";
#endif
  } else {
    std::cerr << "Error: Unsupported interrupt number: " << interruptNumber
              << "\n";
  }
}

llvm::Value* LLVMIRGen::handleDestinationMemory(MemoryNode* destMem) {
  llvm::Value* baseAddr = nullptr;
  llvm::Value* offsetVal = nullptr;
  if (!destMem->base.empty()) {
    // check if it is a predefined global variable
    // and create it if not found, for some cases you can find
    // it in the .bss section which defines the uninitialized data section
    if (!getNamedValue(destMem->base)) {
      std::cerr << "Warning: Base register '" << destMem->base
                << "' not allocated. Allocating now.\n";
      // initialize the global variable
      llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
          module,
          llvm::Type::getInt32Ty(context),
          false,
          llvm::GlobalValue::ExternalLinkage,
          llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
          destMem->base);
      globalNamedValues[destMem->base] = globalVar; // registered globally
    }
    baseAddr = getNamedValue(destMem->base);
    baseAddr = builder.CreateLoad(
        llvm::Type::getInt32Ty(context), baseAddr, destMem->base);
  }
  if (!destMem->offset.empty()) {
    try {
      int offsetInt = std::stoi(destMem->offset);
      offsetVal =
          llvm::ConstantInt::get(context, llvm::APInt(32, offsetInt, true));
    } catch (const std::exception& e) {
      std::cerr << "Error: Invalid memory offset '" << destMem->offset << "'\n";
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
    nodeValue = getNamedValue(srcReg->registerName);

    if (!nodeValue) {
      std::cerr << "Error: Register " << srcReg->registerName << " not found\n";
      return nullptr;
    }
  } else if (nodeType == "mem") {
    auto* memNode = dynamic_cast<MemoryNode*>(inputNode);
    if (!memNode) {
      std::cerr << "Error: Invalid memory operand\n";
      return nullptr;
    }
    llvm::Value* baseAddr = nullptr;
    llvm::Value* offsetVal = nullptr;

    if (!memNode->base.empty()) {
      if (!getNamedValue(memNode->base)) {
        std::cerr << "Error: Base register " << memNode->base
                  << " not allocated\n";
        return nullptr;
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
        std::cerr << "Error: Invalid memory offset '" << memNode->offset
                  << "'\n";
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
  if (node->opcode == "cmp") {
    // the operands should be integers not pointers
    if (lhsValue->getType()->isPointerTy()) {
      lhsValue =
          builder.CreatePtrToInt(lhsValue, llvm::Type::getInt32Ty(context));
    }
    if (rhsValue->getType()->isPointerTy()) {
      rhsValue =
          builder.CreatePtrToInt(rhsValue, llvm::Type::getInt32Ty(context));
    }
    // Ensure both values are of the same type
    llvm::Value* result = builder.CreateSub(lhsValue, rhsValue, "cmp_tmp");

    // Update Zero Flag (ZF)
    ContextCPUState.zeroFlag = builder.CreateICmpEQ(
        result, llvm::ConstantInt::get(lhsValue->getType(), 0), "zeroFlag");

    // Update Sign Flag (SF)
    ContextCPUState.signFlag = builder.CreateICmpSLT(
        result, llvm::ConstantInt::get(lhsValue->getType(), 0), "signFlag");

    // Update Carry Flag (CF)
    ContextCPUState.carryFlag =
        builder.CreateICmpULT(lhsValue, rhsValue, "carryFlag");

    // Update Overflow Flag (OF)
    llvm::Value* lhsSign = builder.CreateICmpSLT(
        lhsValue, llvm::ConstantInt::get(lhsValue->getType(), 0));
    llvm::Value* rhsSign = builder.CreateICmpSLT(
        rhsValue, llvm::ConstantInt::get(rhsValue->getType(), 0));
    llvm::Value* resultSign = builder.CreateICmpSLT(
        result, llvm::ConstantInt::get(lhsValue->getType(), 0));
    ContextCPUState.overflowFlag = builder.CreateXor(
        builder.CreateXor(lhsSign, rhsSign), resultSign, "overflowFlag");
  } else if (node->opcode == "test") {
    if (lhsValue->getType()->isPointerTy()) {
      lhsValue =
          builder.CreatePtrToInt(lhsValue, llvm::Type::getInt32Ty(context));
    }
    if (rhsValue->getType()->isPointerTy()) {
      rhsValue =
          builder.CreatePtrToInt(rhsValue, llvm::Type::getInt32Ty(context));
    }
    llvm::Value* result = builder.CreateAnd(lhsValue, rhsValue, "test_tmp");

    // Update Zero Flag (ZF): Set if result is zero
    ContextCPUState.zeroFlag = builder.CreateICmpEQ(
        result, llvm::ConstantInt::get(lhsValue->getType(), 0), "zeroFlag");

    // Update Sign Flag (SF): Set if result is negative
    ContextCPUState.signFlag = builder.CreateICmpSLT(
        result, llvm::ConstantInt::get(lhsValue->getType(), 0), "signFlag");

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
  llvm::Function* function = builder.GetInsertBlock()->getParent();

  ASTNode* branchLabel = node->operands[0].get();
  if (!branchLabel) {
    std::cerr << "Error: No label provided for: " << node->opcode
              << " instruction\n";
    return;
  }

  auto* labelNode = dynamic_cast<MemoryNode*>(branchLabel);
  if (!labelNode) {
    std::cerr << "Error: Invalid label type for: " << node->opcode
              << " instruction\n";
    return;
  }

  std::string targetLabel = entryBlockNames[labelNode->base];
  if (targetLabel.empty()) {
    std::cerr << "Error: Empty label for: " << node->opcode << " instruction\n";
    return;
  }

  // Ensure the label exists in the labelMap
  if (labelMap.find(targetLabel) == labelMap.end()) {
    std::cerr << "Error: Target label '" << targetLabel
              << "' not found in labelMap\n";
    return;
  }

  llvm::BasicBlock* trueBB = labelMap[targetLabel];

  std::string opcode = node->opcode;
  llvm::Value* condition = nullptr;

  // Determine the condition based on the opcode
  if (opcode == "je") {
    condition = ContextCPUState.zeroFlag;
  } else if (opcode == "jne") {
    condition = builder.CreateICmpNE(
        ContextCPUState.zeroFlag,
        llvm::ConstantInt::getTrue(context),
        "cmp_ne");
  } else if (opcode == "jnz") {
    condition = builder.CreateICmpNE(
        ContextCPUState.zeroFlag,
        llvm::ConstantInt::getFalse(context),
        "cmp_nz");
  } else if (opcode == "jg") {
    condition = builder.CreateAnd(
        builder.CreateICmpEQ(
            ContextCPUState.overflowFlag, ContextCPUState.signFlag, "cmp_eq"),
        builder.CreateICmpNE(
            ContextCPUState.zeroFlag,
            llvm::ConstantInt::getTrue(context),
            "cmp_ne"),
        "jg_condition");
  } else if (opcode == "jge") {
    condition = builder.CreateICmpEQ(
        ContextCPUState.overflowFlag,
        ContextCPUState.signFlag,
        "jge_condition");
  } else if (opcode == "jl") {
    condition = builder.CreateICmpNE(
        ContextCPUState.overflowFlag, ContextCPUState.signFlag, "jl_condition");
  } else if (opcode == "jle") {
    condition = builder.CreateOr(
        builder.CreateICmpNE(
            ContextCPUState.overflowFlag,
            ContextCPUState.signFlag,
            "jl_condition"),
        ContextCPUState.zeroFlag,
        "jle_condition");
  } else if (opcode == "jc") {
    condition = ContextCPUState.carryFlag;
  } else if (opcode == "jnc") {
    condition = builder.CreateICmpNE(
        ContextCPUState.carryFlag,
        llvm::ConstantInt::getTrue(context),
        "jnc_condition");
  } else if (opcode == "jmp") {
    builder.CreateBr(trueBB);
    builder.SetInsertPoint(trueBB); // Set insertion point to target block
    return;
  } else {
    std::cerr << "Error: Unsupported branching opcode " << opcode << "\n";
    return;
  }

  if (!condition) {
    std::cerr << "Error: Failed to generate condition for branch instruction\n";
    return;
  }

  static llvm::BasicBlock* commonFalseBB = nullptr;
  if (!commonFalseBB) {
    commonFalseBB =
        llvm::BasicBlock::Create(context, "FalseBasicBlock", function);
  }

  // Create the conditional branch
  builder.CreateCondBr(condition, trueBB, commonFalseBB);

  // Update the insertion point to the false block
  builder.SetInsertPoint(commonFalseBB);
}

// when we have a function call
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
      std::cerr << "Error: Failed to evaluate operand " << i << "\n";
      return;
    }
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
  llvm::CallInst* call = builder.CreateCall(calleeFunction, args);
  call->setTailCall(false);

  // Optional: use the result if not void
  if (!calleeFunction->getReturnType()->isVoidTy()) {
    llvm::Value* result = call;
    // Store result to stack or variable table
  }
}

void LLVMIRGen::handleDivInstructionNode(InstructionNode* node) {
  if (node->operands.size() != 1) {
    std::cerr << "Error: div expects a single operand (the divisor).\n";
    return;
  }

  ASTNode* divisorNode = node->operands[0].get();
  std::string divisorType = check_operandType(divisorNode);
  llvm::Value* divisor = castInputTypes(divisorNode, divisorType);

  if (divisor->getType()->isPointerTy()) {
    divisor = builder.CreatePtrToInt(
        divisor, llvm::Type::getInt32Ty(context), "divisor_int");
  }

  llvm::Value* eaxPtr = namedValues["eax"];
  llvm::Value* edxPtr = namedValues["edx"];
  // keep the track of changed types to cast them to the initial type
  std::vector<std::string> changedTypes;
  if (!eaxPtr || !eaxPtr->getType()->isPointerTy()) {
    std::cerr
        << "Warning: EAX is not initialized as a pointer. Allocating it.\n";
    llvm::Value* allocaInst =
        builder.CreateAlloca(llvm::Type::getInt32Ty(context), nullptr, "eax");
    builder.CreateStore(
        llvm::ConstantInt::get(context, llvm::APInt(32, 0)), allocaInst);
    namedValues["eax"] = allocaInst;
    eaxPtr = allocaInst;
    changedTypes.push_back("eax");
  }

  if (!edxPtr || !edxPtr->getType()->isPointerTy()) {
    std::cerr
        << "Warning: EDX is not initialized as a pointer. Allocating it.\n";
    llvm::Value* allocaInst =
        builder.CreateAlloca(llvm::Type::getInt32Ty(context), nullptr, "edx");
    builder.CreateStore(
        llvm::ConstantInt::get(context, llvm::APInt(32, 0)), allocaInst);
    namedValues["edx"] = allocaInst;
    edxPtr = allocaInst;
    changedTypes.push_back("edx");
  }

  // Load EAX and EDX (assumed initialized earlier)
  llvm::Value* eaxVal =
      builder.CreateLoad(llvm::Type::getInt32Ty(context), eaxPtr, "load_eax");
  llvm::Value* edxVal =
      builder.CreateLoad(llvm::Type::getInt32Ty(context), edxPtr, "load_edx");

  // Combine EDX:EAX into a 64-bit dividend
  llvm::Value* edxExt =
      builder.CreateZExt(edxVal, llvm::Type::getInt64Ty(context), "zext_edx");
  llvm::Value* eaxExt =
      builder.CreateZExt(eaxVal, llvm::Type::getInt64Ty(context), "zext_eax");

  llvm::Value* dividend = builder.CreateShl(
      edxExt, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 32));
  dividend = builder.CreateOr(dividend, eaxExt, "dividend");

  // Zero-check
  llvm::Value* isZero = builder.CreateICmpEQ(
      divisor, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));
  llvm::Function* parentFunc = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock* divOkBB =
      llvm::BasicBlock::Create(context, "div.ok", parentFunc);
  llvm::BasicBlock* divErrBB =
      llvm::BasicBlock::Create(context, "div.err", parentFunc);

  builder.CreateCondBr(isZero, divErrBB, divOkBB);

  builder.SetInsertPoint(divErrBB);
  builder.CreateCall(
      getPrintFunction(),
      {builder.CreateGlobalStringPtr("Divide by zero error\n")});
  builder.CreateUnreachable();

  builder.SetInsertPoint(divOkBB);

  llvm::Value* divisor64 = builder.CreateZExt(
      divisor, llvm::Type::getInt64Ty(context), "zext_divisor");

  llvm::Value* quotient = builder.CreateUDiv(dividend, divisor64, "quotient");
  llvm::Value* remainder = builder.CreateURem(dividend, divisor64, "remainder");

  // Truncate results back to 32-bit and store in EAX and EDX
  llvm::Value* q32 =
      builder.CreateTrunc(quotient, llvm::Type::getInt32Ty(context), "quot32");
  llvm::Value* r32 =
      builder.CreateTrunc(remainder, llvm::Type::getInt32Ty(context), "rem32");
  // before storing the values, we need to check if they are pointers
  if (std::find(changedTypes.begin(), changedTypes.end(), "eax") !=
      changedTypes.end()) {
    q32 =
        builder.CreatePtrToInt(q32, llvm::Type::getInt32Ty(context), "q32_int");
  }
  if (std::find(changedTypes.begin(), changedTypes.end(), "edx") !=
      changedTypes.end()) {
    r32 =
        builder.CreatePtrToInt(r32, llvm::Type::getInt32Ty(context), "r32_int");
  }
  // Store the results back to EAX and EDX
  builder.CreateStore(q32, namedValues["eax"]);
  builder.CreateStore(r32, namedValues["edx"]);
}

llvm::FunctionCallee LLVMIRGen::getPrintFunction() {
  llvm::FunctionType* ft = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(context),
      llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context)),
      false);

  return module.getOrInsertFunction("puts", ft);
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
  llvm::Type* int32Ty = llvm::Type::getInt32Ty(context);

  bool isPush = node->opcode == "push";
  bool isPop = node->opcode == "pop";

  if (!isPush && !isPop) {
    std::cerr << "[Error] Unsupported stack opcode: " << node->opcode << "\n";
    return;
  }

  if (node->operands.empty()) {
    std::cerr << "[Error] No operand found for '" << node->opcode << "'\n";
    return;
  }

  RegisterNode* regNode = dynamic_cast<RegisterNode*>(node->operands[0].get());
  if (!regNode) {
    std::cerr << "[Error] Invalid register type for '" << node->opcode << "'\n";
    return;
  }

  llvm::Type* operandType = nullptr;
  llvm::Value* valueToPush = nullptr;
  if (isPush) {
    valueToPush = getNamedValue(regNode->registerName);
    if (!valueToPush) {
      std::cerr << "[Error] Register '" << regNode->registerName
                << "' not found for 'push'\n";
      return;
    }
    operandType = valueToPush->getType();
  } else {
    operandType = getLLVMTypeForRegister(regNode->registerName);
    if (!operandType) {
      std::cerr << "[Error] Failed to infer type for 'pop' register: "
                << regNode->registerName << "\n";
      return;
    }
  }
  llvm::AllocaInst* rsp = nullptr;
  llvm::AllocaInst* stackMem = nullptr;

  if (operandType->isIntegerTy(32)) {
    rsp = rspIntPerFunction[currentFunc];
    stackMem = stackMemIntPerFunction[currentFunc];
  } else if (operandType->isPointerTy()) {
    rsp = rspPtrPerFunction[currentFunc];
    stackMem = stackMemPtrPerFunction[currentFunc];
  } else {
    std::cerr << "[Error] Unsupported operand type for stack: ";
    operandType->print(llvm::errs());
    std::cerr << "\n";
    return;
  }

  if (!rsp || !stackMem) {
    std::cerr << "[Error] rsp or stackMem is nullptr for function: "
              << currentFunc->getName().str() << "\n";
    return;
  }

  llvm::Value* idx = builder.CreateLoad(
      int32Ty, rsp, "load_rsp_" + currentFunc->getName().str());

  if (isPush) {
    llvm::Value* gep = builder.CreateInBoundsGEP(
        stackMem->getAllocatedType(),
        stackMem,
        {llvm::ConstantInt::get(int32Ty, 0), idx},
        "stack_idx_" + currentFunc->getName().str());
    builder.CreateStore(valueToPush, gep);

    llvm::Value* newRsp =
        builder.CreateAdd(idx, llvm::ConstantInt::get(int32Ty, 1));
    builder.CreateStore(newRsp, rsp);
  } else { // pop
    llvm::Value* newRsp =
        builder.CreateSub(idx, llvm::ConstantInt::get(int32Ty, 1));
    builder.CreateStore(newRsp, rsp);

    llvm::Value* gep = builder.CreateInBoundsGEP(
        stackMem->getAllocatedType(),
        stackMem,
        {llvm::ConstantInt::get(int32Ty, 0), newRsp},
        "stack_idx_" + currentFunc->getName().str());

    llvm::Value* popVal = builder.CreateLoad(operandType, gep);
    namedValues[regNode->registerName] = popVal;
  }
}

void LLVMIRGen::handleUnaryInstructionNode(
    InstructionNode* node,
    const std::string& opType) {
  if (node->operands.empty()) {
    std::cerr << "Error: Unary instruction requires at least one operand\n";
    return;
  }

  ASTNode* operandNode = node->operands[0].get();
  if (!operandNode) {
    std::cerr << "Error: Operand node is null\n";
    return;
  }

  std::string operandType = check_operandType(operandNode);

  llvm::Value* loadedValue = nullptr;
  llvm::Value* operandPtr = nullptr;

  if (operandType == "reg") {
    auto* regNode = dynamic_cast<RegisterNode*>(operandNode);
    llvm::Value* regValue = getNamedValue(regNode->registerName);

    if (!regValue) {
      operandPtr = builder.CreateAlloca(
          llvm::Type::getInt32Ty(context), nullptr, regNode->registerName);
      namedValues[regNode->registerName] = operandPtr;
      loadedValue = builder.CreateLoad(
          llvm::Type::getInt32Ty(context),
          operandPtr,
          regNode->registerName + "_load");
    } else {
      if (regValue->getType()->isPointerTy()) {
        operandPtr = regValue; // Already a pointer, good
        loadedValue = builder.CreateLoad(
            llvm::Type::getInt32Ty(context),
            operandPtr,
            regNode->registerName + "_load");
      } else if (regValue->getType()->isIntegerTy(32)) {
        // It's an integer but we need to treat it as pointer
        operandPtr = builder.CreateIntToPtr(
            regValue,
            llvm::PointerType::get(llvm::Type::getInt32Ty(context), 0),
            regNode->registerName + "_ptrcast");
        loadedValue = builder.CreateLoad(
            llvm::Type::getInt32Ty(context),
            operandPtr,
            regNode->registerName + "_load");
      } else {
        std::cerr << "Unsupported register type for unary operation\n";
        return;
      }
    }
  } else if (operandType == "mem") {
    auto* memNode = dynamic_cast<MemoryNode*>(operandNode);
    operandPtr = handleDestinationMemory(memNode);
    loadedValue = builder.CreateLoad(
        llvm::Type::getInt32Ty(context), operandPtr, "mem_load");
  } else {
    std::cerr << "Error: Unsupported operand type for unary instruction: "
              << operandType << "\n";
    return;
  }

  // Now apply the operation
  if (opType == "inc") {
    auto* result = builder.CreateAdd(
        loadedValue,
        llvm::ConstantInt::get(context, llvm::APInt(32, 1)),
        "inc");
    builder.CreateStore(result, operandPtr);
    return;
  } else if (opType == "dec") {
    auto* result = builder.CreateSub(
        loadedValue,
        llvm::ConstantInt::get(context, llvm::APInt(32, 1)),
        "dec");
    builder.CreateStore(result, operandPtr);
    return;
  } else if (opType == "neg") {
    auto* result = builder.CreateNeg(loadedValue, "neg");
    builder.CreateStore(result, operandPtr);
    return;
  } else {
    std::cerr << "Error: Unknown unary operation type: " << opType << "\n";
    return;
  }
}
