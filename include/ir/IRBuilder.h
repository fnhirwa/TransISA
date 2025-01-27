#ifndef IRBUILDER_H
#define IRBUILDER_H

#include "ir/IRNode.h"
#include "parser/AST.h"

class IRBuilder {
 public:
  IRNode* build(ASTNode* ast);

 private:
  IRNode* builInstruction(InstructionNode* instruction);
  IRNode* buildOperand(OperandNode* operand);
};

#endif // IRBUILDER_H