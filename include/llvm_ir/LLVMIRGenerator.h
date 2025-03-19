#ifndef LLVMIRGENERATOR_H
#define LLVMIRGENERATOR_H

#include <functional>
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
  // Define lookup table mapping x86 opcodes to LLVM IR generation functions
  std::unordered_map<
      std::string,
      std::function<
          llvm::Value*(llvm::IRBuilder<>&, llvm::Value*, llvm::Value*)>>
      binOpTable = {
          {"add",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateAdd(lhs, rhs, "addtmp");
           }},
          {"sub",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateSub(lhs, rhs, "subtmp");
           }},
          {"mul",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateMul(lhs, rhs, "multmp");
           }},
          {"div",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateSDiv(lhs, rhs, "divtmp"); // Signed division
           }},
          {"and",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateAnd(lhs, rhs, "andtmp");
           }},
          {"or",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateOr(lhs, rhs, "ortmp");
           }},
          {"xor",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateXor(lhs, rhs, "xortmp");
           }},
          {"shl",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateShl(lhs, rhs, "shltmp");
           }},
          {"shr",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateLShr(
                 lhs, rhs, "shrtmp"); // Logical shift right
           }},
          {"sar",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateAShr(
                 lhs, rhs, "sartmp"); // Arithmetic shift right
           }},
  };

 public:
  LLVMIRGen() : module("TransISA", context), builder(context) {}
  llvm::Module* generateIR(std::unique_ptr<ASTNode>& root);
  void visitGlobalVariableNode(GlobalVariableNode* node);
  void visitFunctionNode(FunctionNode* node);
  void visitBasicBlockNode(BasicBlockNode* node);
  void visitInstructionNode(InstructionNode* node);

  // different instruction set
  void handleBinaryOpNode(InstructionNode* node);
  void handleMovInstructionNode(InstructionNode* node);
  void handleLeaInstructionNode(InstructionNode* node);
  void handleSyscallInstructionNode(InstructionNode* node);
  void handleIntInstructionNode(InstructionNode* node);
  void handleRetInstructionNode(InstructionNode* node);

  // some memory related functions
  llvm::Value* handleDestinationMemory(MemoryNode* destMem);
  llvm::Value* handleSourceMemory(MemoryNode* srcMem);
};
#endif // LLVMIRGENERATOR_H
