#ifndef LLVMIRGENERATOR_H
#define LLVMIRGENERATOR_H

#include "ir/IRNode.h"
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>

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
