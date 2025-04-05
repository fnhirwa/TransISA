#ifndef LLVMIRGENERATOR_H
#define LLVMIRGENERATOR_H

#include <cstddef>
#include <functional>
#include <iostream>
#include <unordered_map>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "parser/Parser.h"

struct TrackCPUState {
  llvm::Value* zeroFlag; // Set if result is zero
  llvm::Value* signFlag; // Set if result is negative
  llvm::Value* carryFlag; // For unsigned comparisons
  llvm::Value* overflowFlag; // For signed comparisons
  TrackCPUState()
      : zeroFlag(nullptr), signFlag(nullptr), carryFlag(nullptr),
        overflowFlag(nullptr) {}
};

class LLVMIRGen {
 private:
  llvm::LLVMContext context;
  llvm::Module module;
  llvm::IRBuilder<> builder;
  std::unordered_map<std::string, llvm::Value*> namedValues;
  std::unordered_map<std::string, llvm::BasicBlock*> labelMap;
  // predefined functions for lookup
  std::unordered_map<std::string, llvm::Function*> definedFunctionsMap;
  std::unordered_map<std::string, std::string> entryBlockNames;
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
  // Lookup table for comparison operations (cmp, test, etc.)
  // This is a simplified version and may not cover all x86 comparison
  // instructions.
  std::unordered_map<
      std::string,
      std::function<
          llvm::Value*(llvm::IRBuilder<>&, llvm::Value*, llvm::Value*)>>
      cmpOpTable = {
          {"cmp",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateICmpEQ(lhs, rhs, "cmptmp");
           }},
          {"test",
           [](llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs) {
             return builder.CreateICmpEQ(lhs, rhs, "testtmp");
           }},
  };

  // Table for jump instructions for O(1) lookup.
  std::unordered_map<std::string, std::string> jumpOpTable = {
      {"jmp", "unconditional"},
      {"je", "equal"},
      {"jne", "not_equal"},
      {"jg", "greater"},
      {"jge", "greater_equal"},
      {"jl", "less"},
      {"jle", "less_equal"},
      {"ja", "above"},
      {"jz", "zero"},
      {"jnz", "not_zero"},
      {"jo", "overflow"},
      {"jno", "no_overflow"},
      {"js", "sign"},
      {"jns", "no_sign"},
      {"jae", "above_equal"},
      {"jb", "below"},
      {"jbe", "below_equal"},
  };
  TrackCPUState ContextCPUState;

 public:
  LLVMIRGen() : module("TransISA", context), builder(context) {}
  llvm::Module* generateIR(std::unique_ptr<ASTNode>& root);
  llvm::Module* getModule() {
    return &module;
  }
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
  void handlCompareInstructionNode(InstructionNode* node);
  void handleBranchingInstructions(InstructionNode* node);
  void handleLoopInstructionNode(InstructionNode* node);
  void handleCallInstructionNode(InstructionNode* node);

  // some memory related functions
  llvm::Value* handleDestinationMemory(MemoryNode* destMem);
  llvm::Value* handleSourceMemory(MemoryNode* srcMem);
  llvm::Value* castInputTypes(ASTNode* inputNode, const std::string nodeType);
};
#endif // LLVMIRGENERATOR_H
