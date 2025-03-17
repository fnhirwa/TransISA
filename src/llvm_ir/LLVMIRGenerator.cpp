#include "llvm_ir/LLVMIRGenerator.h"
#include <iostream>
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Verifier.h"

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
}

void LLVMIRGen::visitFunctionNode(FunctionNode* node) {
  llvm::FunctionType* funcType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);
  llvm::Function* function = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, node->name, &module);

  llvm::BasicBlock* entryBB =
      llvm::BasicBlock::Create(context, "entry", function);
  builder.SetInsertPoint(entryBB); // Ensure all instructions go into this block

  for (const auto& block : node->basicBlocks) {
    if (block) {
      visitBasicBlockNode(dynamic_cast<BasicBlockNode*>(block.get()));
    }
  }

  // If function needs a return, explicitly add one at the end
  builder.CreateRetVoid();
}

void LLVMIRGen::visitBasicBlockNode(BasicBlockNode* node) {
  llvm::Function* function = builder.GetInsertBlock()->getParent();
  if (!function) {
    std::cerr << "Error: No parent function for the basic block\n";
    return;
  }

  // Do NOT create a new BasicBlock
  for (const auto& instr : node->instructions) {
    visitInstructionNode(dynamic_cast<InstructionNode*>(instr.get()));
  }
}

std::string check_operandType(ASTNode* operand) {
  if (auto* reg = dynamic_cast<RegisterNode*>(operand)) {
    return "reg";
  } else if (auto* mem = dynamic_cast<MemoryNode*>(operand)) {
    return "mem";
  } else if (auto* intLit = dynamic_cast<IntLiteralNode*>(operand)) {
    return "int";
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
    std::cerr << "Error: Register " << lhsReg->registerName
              << " is not initialized.\n";
    return;
  }

  llvm::Value* rhs = nullptr;
  std::string rhsType = check_operandType(rhsNode);

  if (rhsType == "int") {
    auto* intLit = dynamic_cast<IntLiteralNode*>(rhsNode);
    rhs = llvm::ConstantInt::get(context, llvm::APInt(32, intLit->value, true));
  } else if (rhsType == "reg") {
    auto* rhsReg = dynamic_cast<RegisterNode*>(rhsNode);
    rhs = namedValues[rhsReg->registerName];
    if (!rhs) {
      std::cerr << "Error: Register " << rhsReg->registerName
                << " is not initialized.\n";
      return;
    }
  } else {
    std::cerr
        << "Error: Unsupported right operand type for binary operation.\n";
    return;
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
    builder.CreateStore(srcValue, destPtr);
  } else {
    std::cerr << "Error: Destination operand must be a register\n";
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
      effectiveAddr = builder.CreateBitCast(
          globalVar, llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0));
    } else {
      std::cerr << "Error: Global variable '" << memNode->base
                << "' not found\n";
      return;
    }
  } else {
    std::cerr << "Error: Source of 'lea' must be a memory operand\n";
    return;
  }

  // Store in namedValues and ensure it exists in IR
  llvm::AllocaInst* ecxVar = builder.CreateAlloca(
      llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0),
      0,
      destReg->registerName);
  builder.CreateStore(effectiveAddr, ecxVar);

  namedValues[destReg->registerName] = ecxVar; // Keep track in map
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

#elif defined(__aarch64__)
    // Get the syscall number (stored in "eax" in x86, but should be "x8" in
    // ARM64)
    llvm::Value* syscallNumber = namedValues["eax"];
    if (!syscallNumber) {
      std::cerr << "Error: EAX not set before system call\n";
      return;
    }

    // Assign syscall number to x8 (for ARM64)
    namedValues["x8"] = syscallNumber;

    // Map arguments from x86 registers to ARM64 registers
    llvm::Value* arg0 = namedValues.count("ebx")
        ? namedValues["ebx"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    llvm::Value* arg1 = namedValues.count("ecx")
        ? namedValues["ecx"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));
    llvm::Value* arg2 = namedValues.count("edx")
        ? namedValues["edx"]
        : llvm::ConstantInt::get(context, llvm::APInt(64, 0));

    // Store arguments in correct ARM64 registers
    namedValues["x0"] = arg0;
    namedValues["x1"] = arg1;
    namedValues["x2"] = arg2;

    // Inline assembly for `svc #0`
    llvm::InlineAsm* svcAsm = llvm::InlineAsm::get(
        llvm::FunctionType::get(llvm::Type::getVoidTy(context), {}, false),
        "svc #0x80",
        "",
        true /* hasSideEffects */
    );
    builder.CreateCall(svcAsm);

#else
    std::cerr << "Error: Unsupported architecture\n";
#endif
  } else {
    std::cerr << "Error: Unsupported interrupt number: " << interruptNumber
              << "\n";
  }
}

void LLVMIRGen::handleSyscallInstructionNode(InstructionNode* node) {}

void LLVMIRGen::handleRetInstructionNode(InstructionNode* node) {}
