#ifndef LLVMIRGENERATOR_H
#define LLVMIRGENERATOR_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include "ir/IRNode.h"

class LLVMIRGenerator {
 public:
  LLVMIRGenerator();
  std::unique_ptr<llvm::Module> generate(IRNode* ir);

 private:
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::IRBuilder<>> builder;

  void generateInstruction(IRInstruction* instruction);
  llvm::Value* generateOperand(IROperand* operand);
};

#endif // LLVMIRGENERATOR_H
