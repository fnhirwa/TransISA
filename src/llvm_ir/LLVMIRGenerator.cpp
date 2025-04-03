#include "llvm_ir/LLVMIRGenerator.h"
#include <iostream>
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Verifier.h"

// This module object is used to store all the IR generated
// It is a container for all the IR instructions
llvm::Module* LLVMIRGen::generateIR(std::unique_ptr<ASTNode>& root) {
  for (const auto& child : root->children) {
    if (auto* var = dynamic_cast<GlobalVariableNode*>(child.get())) {
      visitGlobalVariableNode(var);
    } else if (auto* func = dynamic_cast<FunctionNode*>(child.get())) {
      visitFunctionNode(func);
    }
  }
  return &module;
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
  namedValues[node->name] = globalPtr;
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

  llvm::BasicBlock* entryBB = nullptr;
  bool isFirstBlock = true;
  // Declare all labels (basic blocks) upfront without generating IR
  // instructions
  // std::vector<std::pair<std::string, BasicBlockNode*>> labelVector;
  // for (const auto& labelPair : Parser::parserLabelMap) {
  //   const std::string& labelName = labelPair.first;

  //   // Check if the label is not in parserFunctionMap
  //   if (Parser::parserFunctionMap.find(labelName) ==
  //   Parser::parserFunctionMap.end()) {
  //     labelVector.push_back(std::make_pair(labelName,
  //     Parser::parserLabelMap[labelName]));
  //   }
  // }
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
  }
  // Visit each block and generate the IR
  for (const auto& block : node->basicBlocks) {
    if (block) {
      auto* basicBlockNode = dynamic_cast<BasicBlockNode*>(block.get());
      std::string basicBlockName = entryBlockNames[basicBlockNode->label];

      if (labelMap.find(basicBlockName) != labelMap.end()) {
        llvm::BasicBlock* bb = labelMap[basicBlockName];
        builder.SetInsertPoint(bb);
        visitBasicBlockNode(basicBlockNode);
      } else {
        std::cerr << "Error: Block '" << basicBlockNode->label
                  << "' not found in labelMap.\n";
      }
    }
  }

  // Ensure the function has a terminating instruction
  if (!builder.GetInsertBlock()->getTerminator()) {
    builder.CreateRetVoid();
  }
}

void LLVMIRGen::visitBasicBlockNode(BasicBlockNode* node) {
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
  }
  if (node->opcode == "lea") {
    handleLeaInstructionNode(node);
    return;
  }
  if (node->opcode == "int") {
    handleIntInstructionNode(node);
    return;
  }
  if (binOpTable.find(node->opcode) != binOpTable.end()) {
    handleBinaryOpNode(node);
    return;
  }
  if (cmpOpTable.find(node->opcode) != cmpOpTable.end()) {
    handlCompareInstructionNode(node);
    return;
  }
  if (jumpOpTable.find(node->opcode) != jumpOpTable.end()) {
    handleBranchingInstructions(node);
    return;
  }
}

void LLVMIRGen::handleBinaryOpNode(InstructionNode* node) {
  if (node->operands.size() != 2) {
    std::cerr << "Error: Binary operation requires exactly two operands.\n";
    return;
  }

  ASTNode* lhsNode = node->operands[0].get();
  ASTNode* rhsNode = node->operands[1].get();

  // Get the destination register
  auto* lhsReg = dynamic_cast<RegisterNode*>(lhsNode);
  if (!lhsReg) {
    std::cerr << "Error: Left operand must be a register.\n";
    return;
  }

  llvm::Value* lhs = namedValues[lhsReg->registerName];
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

    llvm::Value* lhs = namedValues[lhsReg->registerName];
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
  if (node->operands.size() != 2) {
    std::cerr << "Invalid 'mov' operands: " << node->operands.size() << "\n";
    return;
  }

  ASTNode* destNode = node->operands[0].get();
  ASTNode* srcNode = node->operands[1].get();

  llvm::Function* function = builder.GetInsertBlock()->getParent();
  if (!function) {
    std::cerr << "Error: No parent function for 'mov' instruction\n";
    return;
  }

  std::string destType = check_operandType(destNode);
  std::string srcType = check_operandType(srcNode);

  llvm::Value* srcValue = nullptr;

  if (srcType == "int") {
    auto* intLit = dynamic_cast<IntLiteralNode*>(srcNode);
    srcValue =
        llvm::ConstantInt::get(context, llvm::APInt(32, intLit->value, true));
  } else if (srcType == "reg") {
    auto* srcReg = dynamic_cast<RegisterNode*>(srcNode);
    srcValue = namedValues[srcReg->registerName];

    if (!srcValue) {
      std::cerr << "Error: Source register " << srcReg->registerName
                << " not found\n";
      return;
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
      if (namedValues.find(memNode->base) == namedValues.end()) {
        std::cerr << "Error: Base register " << memNode->base
                  << " not allocated\n";
        return;
      }
      baseAddr = namedValues[memNode->base];
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
    if (namedValues.find(destReg->registerName) == namedValues.end()) {
      llvm::Value* allocaInst = builder.CreateAlloca(
          llvm::Type::getInt32Ty(context), nullptr, destReg->registerName);
      namedValues[destReg->registerName] = allocaInst;
    }

    llvm::Value* destPtr = namedValues[destReg->registerName];
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
      if (namedValues.find(destReg->registerName) == namedValues.end()) {
        // create an alloca for the destination reg if it doesn't exist
        llvm::Type* registerType = llvm::PointerType::get(context, 0);
        llvm::AllocaInst* allocaInst =
            builder.CreateAlloca(registerType, nullptr, destReg->registerName);
        namedValues[destReg->registerName] = allocaInst;
      }
      // store the effective address in the destination register
      llvm::Value* destPtr = namedValues[destReg->registerName];
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
    llvm::Value* syscallNumber = namedValues["eax"];
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

#elif defined(__APPLE__) && defined(__aarch64__)
    // For macOS ARM64, we need to use inline assembly for system calls
    // Create a function type for our inline assembly
    std::vector<llvm::Type*> asmParamTypes = {
        llvm::Type::getInt64Ty(context), // fd
        llvm::Type::getInt64Ty(context), // buffer
        llvm::Type::getInt64Ty(context) // length
    };
    llvm::FunctionType* asmFuncType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context), // Return type (void)
        asmParamTypes,
        false // isVarArg
    );

    // Helper function to load and convert values to i64
    auto loadAndConvertToInt64 = [&](const std::string& regName,
                                     llvm::Type* regType) -> llvm::Value* {
      if (namedValues.count(regName)) {
        llvm::Value* regValue = namedValues[regName];
        if (regValue->getType()->isPointerTy()) {
          // If the value is a pointer, load it first
          llvm::Value* loadedValue = builder.CreateLoad(regType, regValue);
          return builder.CreateZExtOrTrunc(
              loadedValue, llvm::Type::getInt64Ty(context));
        } else if (regValue->getType()->isIntegerTy()) {
          // If the value is an integer, zero-extend it to i64
          return builder.CreateZExtOrTrunc(
              regValue, llvm::Type::getInt64Ty(context));
        } else {
          throw std::runtime_error("Unsupported type for register: " + regName);
        }
      } else {
        // Provide a default value if the register is not found
        return llvm::ConstantInt::get(context, llvm::APInt(64, 0));
      }
    };

    // Get arguments for the syscall
    llvm::Value* arg0 = loadAndConvertToInt64(
        "ebx", llvm::Type::getInt32Ty(context)); // File descriptor (stdout)

    llvm::Value* arg1 = nullptr;
    if (namedValues.count("ecx")) {
      // First load the pointer stored in ecx
      llvm::Value* loadedPtr = builder.CreateLoad(
          llvm::PointerType::get(context, 0), // Load as pointer type
          namedValues["ecx"]);
      // Then convert that pointer to an integer
      arg1 = builder.CreatePtrToInt(loadedPtr, llvm::Type::getInt64Ty(context));

    } else {
      arg1 = llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    }

    llvm::Value* arg2 = loadAndConvertToInt64(
        "edx", llvm::Type::getInt32Ty(context)); // Length of the message

    // arg3 esi
    // arg4 edi
    // arg5 ebp

    /*
    For MacOS ARM64:arg0
    x16 = syscall number
    x0 = arg0
    x1 = arg1
    x2 = arg2
    x3 = arg3
    ...

    syscall number = 0x2000004 (write)

    For x86_6 using syscall function
    syscall number = rax
    arg1 = rdi
    arg2 = rsi
    arg3 = rdx
    arg4 = r10
    arg5 = r8
    arg6 = r9
    */

    // Create inline assembly for the syscall
    std::string asmString =
        "mov x0, $0\n"
        "mov x1, $1\n"
        "mov x2, $2\n"
        "mov x16, #4\n" // Base syscall number for write
        "orr x16, x16, #0x2000000\n" // OR with macOS prefix
        "svc #0x80\n"; // Make the syscall

    std::string constraints = "r,r,r";

    // Create the inline assembly call
    llvm::InlineAsm* inlineAsm = llvm::InlineAsm::get(
        asmFuncType,
        asmString,
        constraints,
        true // hasSideEffects
    );

    // Make the call
    std::vector<llvm::Value*> args = {arg0, arg1, arg2};
    builder.CreateCall(inlineAsm, args);

    // Handle exit syscall (assuming this is for int 0x80 with eax = 1)
    // In macOS ARM64, the exit syscall number is 1

    // Get the exit status code (usually in ebx)
    llvm::Value* exitStatus = loadAndConvertToInt64(
        "ebx", llvm::Type::getInt32Ty(context)); // Exit status code

    // Create function type for exit syscall (only needs one parameter)
    std::vector<llvm::Type*> exitAsmParamTypes = {
        llvm::Type::getInt64Ty(context) // exit status
    };
    llvm::FunctionType* exitAsmFuncType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context), // Return type (void)
        exitAsmParamTypes,
        false // isVarArg
    );

    // Create inline assembly for the exit syscall
    std::string exitAsmString =
        "mov x0, $0\n"
        "mov x16, #1\n" // Base syscall number for exit
        "orr x16, x16, #0x2000000\n" // OR with macOS prefix
        "svc #0x80\n"; // Make the syscall
    std::string exitConstraints = "r";

    // Create the inline assembly call
    llvm::InlineAsm* exitInlineAsm = llvm::InlineAsm::get(
        exitAsmFuncType,
        exitAsmString,
        exitConstraints,
        true // hasSideEffects
    );

    // Make the call
    std::vector<llvm::Value*> exitArgs = {exitStatus};
    builder.CreateCall(exitInlineAsm, exitArgs);

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
    if (namedValues.find(destMem->base) == namedValues.end()) {
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
      namedValues[destMem->base] = globalVar; // registered globally
    }
    baseAddr = namedValues[destMem->base];
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

void LLVMIRGen::handleRetInstructionNode(InstructionNode* node) {}

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
    nodeValue = namedValues[srcReg->registerName];

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
      if (namedValues.find(memNode->base) == namedValues.end()) {
        std::cerr << "Error: Base register " << memNode->base
                  << " not allocated\n";
        return nullptr;
      }
      baseAddr = namedValues[memNode->base];
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

void LLVMIRGen::handlCompareInstructionNode(InstructionNode* node) {
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
  llvm::Function* currentFunction = builder.GetInsertBlock()->getParent();
  ASTNode* callee = node->operands[0].get();
  if (!callee) {
    std::cerr << "Error: No Function provided for call Instruction\n";
    return;
  }

  auto* calleeNode = dynamic_cast<MemoryNode*>(callee);
  if (!calleeNode) {
    std::cerr << "Error: Invalid function type for: " << node->opcode
              << " instruction\n";
    return;
  }

  std::string targetFunctionName = calleeNode->base;
  if (targetFunctionName.empty()) {
    std::cerr << "Error: No function specified for: " << node->opcode
              << " instruction\n";
    return;
  }
  llvm::Function* calleeFunction = nullptr;
  // check if the function is predefined in
  if (definedFunctionsMap.find(targetFunctionName) !=
      definedFunctionsMap.end()) {
    calleeFunction = definedFunctionsMap[targetFunctionName];
  } else {
    llvm::FunctionType* funcType =
        llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);
    calleeFunction = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, targetFunctionName, module);
    definedFunctionsMap[targetFunctionName] = calleeFunction;
  }

  llvm::CallInst* callInst = builder.CreateCall(calleeFunction);
  callInst->setTailCall(false);
}

void LLVMIRGen::handleLoopInstructionNode(InstructionNode* node) {}