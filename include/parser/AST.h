#ifndef AST_H
#define AST_H

#include <string>
#include <variant>
#include <vector>
// The ASTNode struct is used as the base class for all nodes in the abstract
// syntax tree. The InstructionNode struct represents an instruction node with
// an instruction name and a list of operands. The RegisterNode struct
// represents a register node with a register name. The ImmediateNode struct
// represents an immediate node with an integer value. The OperandNode struct
// represents an operand node with a name and a value that can be a register,
// immediate, integer, or float.
struct ASTNode {
  virtual ~ASTNode() = default;
};

struct InstructionNode : ASTNode {
  std::string instruction;
  std::vector<ASTNode*> operands;
};

struct RegisterNode : ASTNode {
  std::string registerName;
};

struct ImmediateNode : ASTNode {
  int value;
};

struct OperandNode : ASTNode {
  std::string name;
  std::variant<RegisterNode*, ImmediateNode*, int, float> value;
};

#endif // AST_H
