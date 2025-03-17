#ifndef LLVMIRGENERATOR_H
#define LLVMIRGENERATOR_H

#include <unordered_map>
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "parser/AST.h"

class LLVMIRGen {
 private:
  llvm::LLVMContext context;
  llvm::Module module;
  llvm::IRBuilder<> builder;
  std::unordered_map<std::string, llvm::Value*> namedValues;

 public:
  LLVMIRGen() : module("TransISA", context), builder(context) {}
  llvm::Module* generateIR(std::unique_ptr<ASTNode>& root);

  void visitGlobalVariableNode(GlobalVariableNode* node);
  void visitFunctionNode(FunctionNode* node);
  void visitBasicBlockNode(BasicBlockNode* node);
  void visitInstructionNode(InstructionNode* node);
};
#endif // LLVMIRGENERATOR_H
