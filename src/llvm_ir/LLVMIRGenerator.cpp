#include "llvm_ir/LLVMIRGenerator.h"
#include <iostream>
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
  builder.SetInsertPoint(entryBB); // Set inside function

  for (const auto& block : node->basicBlocks) {
    visitBasicBlockNode(dynamic_cast<BasicBlockNode*>(block.get()));
  }
}

void LLVMIRGen::visitBasicBlockNode(BasicBlockNode* node) {
  llvm::Function* function = builder.GetInsertBlock()->getParent();
  if (!function) {
    std::cerr << "Error: No parent function for the basic block\n";
    return;
  }

  llvm::BasicBlock* bb =
      llvm::BasicBlock::Create(context, node->label, function);
  builder.SetInsertPoint(bb); // Now it's inside a function

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
      llvm::Value* ptr =
          builder.CreateIntToPtr(memAddr, i32PtrType, "ptr_cast");

      srcValue =
          builder.CreateLoad(llvm::Type::getInt32Ty(context), ptr, "mem_load");
    } else {
      std::cerr << "Error: Invalid source operand for 'mov' instruction\n";
      return;
    }

    if (destType == "reg") {
      auto* destReg = dynamic_cast<RegisterNode*>(destNode);
      namedValues[destReg->registerName] = srcValue;
    } else {
      std::cerr << "Error: Destination operand must be a register\n";
    }
  }
}
