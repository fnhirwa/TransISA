#ifndef AST_H
#define AST_H

#include <iostream>
#include <memory>
#include <string>
#include <vector>

/*
The ASTNode struct is used as the base class for all nodes in the abstract
syntax tree. The IntLiteralNode struct represents integer literals, the
BinaryOpNode struct represents binary operations, and the FunctionCallNode
struct represents function calls.

The codegen method is used to generate LLVM IR for the node. The codegen
method for the IntLiteralNode simply creates a constant integer value, while
the codegen method for the BinaryOpNode generates code for the left and right
operands and then performs the specified operation. The codegen method for
the FunctionCallNode generates code for the function arguments and then
calls the function.
*/

// Base class for all AST nodes
class ASTNode {
 public:
  virtual void print(
      int indent = 0) const = 0; // Print function with indentation
  virtual ~ASTNode() = default;
  std::vector<std::unique_ptr<ASTNode>> children;

  void addBasicBlock(std::unique_ptr<ASTNode> block) {
    children.push_back(std::move(block));
  }

 protected:
  void printIndent(int indent) const {
    for (int i = 0; i < indent; ++i)
      std::cout << "  ";
  }
};

// Root node for the AST
class RootNode : public ASTNode {
 public:
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "RootNode:\n";
    for (const auto& child : children) {
      child->print(indent + 1);
    }
  }
};

// Global variable Node (.data section)
class GlobalVariableNode : public ASTNode {
 public:
  std::string name;
  std::string value;
  GlobalVariableNode(const std::string& name, const std::string& value)
      : name(name), value(value) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "GlobalVariable: " << name << " = " << value << "\n";
  }
};

// Function Node (.text section)
class FunctionNode : public ASTNode {
 public:
  std::string name;
  std::vector<std::unique_ptr<ASTNode>> basicBlocks;
  FunctionNode(const std::string name) : name(std::move(name)) {}

  void addBasicBlock(std::unique_ptr<ASTNode> block) {
    basicBlocks.push_back(std::move(block));
  }
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "Function: " << name << "\n";
    for (const auto& block : basicBlocks) {
      block->print(indent + 1);
    }
  }
};

// Basic Block
class BasicBlockNode : public ASTNode {
 public:
  std::string label;
  std::vector<std::unique_ptr<ASTNode>> instructions;
  explicit BasicBlockNode(const std::string& label) : label(label) {}
  void addBasicBlock(std::unique_ptr<ASTNode> instr) {
    instructions.push_back(std::move(instr));
  }

  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "BasicBlock: " << label << "\n";
    for (const auto& instr : instructions) {
      instr->print(indent + 1);
    }
  }
};

// Instruction Node
class InstructionNode : public ASTNode {
 public:
  std::string opcode;
  std::vector<std::unique_ptr<ASTNode>> operands;
  InstructionNode(std::string op, std::vector<std::unique_ptr<ASTNode>> ops)
      : opcode(std::move(op)), operands(std::move(ops)) {}

  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "Instruction: " << opcode << "\n";
    for (const auto& operand : operands) {
      operand->print(indent + 1);
    }
    std::cout << "\n";
  }
};

// Register Node
class RegisterNode : public ASTNode {
 public:
  std::string registerName;
  explicit RegisterNode(const std::string& registerName)
      : registerName(registerName) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "Register: " << registerName << "\n";
  }
};

class MemoryNode : public ASTNode {
 public:
  std::string base;
  std::string offset;
  bool isIndirect;

  explicit MemoryNode(
      std::string base,
      std::string offset = "",
      bool isIndirect = false)
      : base(std::move(base)), offset(std::move(offset)),
        isIndirect(isIndirect) {}

  void print(int indent = 0) const override {
    printIndent(indent);
    if (isIndirect) {
      std::cout << "Memory: [" << base;
      if (!offset.empty())
        std::cout << " + " << offset;
      std::cout << "]\n";
    } else {
      std::cout << "Memory: " << base << "\n";
    }
  }
};

// Integer Literal Node
class IntLiteralNode : public ASTNode {
 public:
  int value;
  explicit IntLiteralNode(int value) : value(value) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "IntLiteral: " << value << "\n";
  }
};

// String Node
class StringNode : public ASTNode {
 public:
  std::string value;
  explicit StringNode(const std::string& value) : value(value) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "String: " << value << "\n";
  }
};

// System Call Node
class SyscallNode : public ASTNode {
 public:
  int sysCallNumber;
  explicit SyscallNode(int sysCallNumber) : sysCallNumber(sysCallNumber) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "Syscall: " << sysCallNumber << "\n";
  }
};

// Label Node
class LabelNode : public ASTNode {
 public:
  std::string label;
  explicit LabelNode(const std::string& label) : label(label) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "Label: " << label << "\n";
  }
};

// Global Node
class GlobalNode : public ASTNode {
 public:
  std::string global;
  explicit GlobalNode(const std::string& global) : global(global) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "Global: " << global << "\n";
  }
};

// BSS Node
class BssNode : public ASTNode {
 public:
  std::string varName;
  std::string directiveName;
  const std::string size;
  explicit BssNode(
      const std::string& varName,
      const std::string& directiveName,
      const std::string& size)
      : varName(varName), directiveName(directiveName), size(size) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "BSS: " << varName << " " << directiveName << " " << size
              << "\n";
  }
};

// Punctuation Node
class PunctuationNode : public ASTNode {
 public:
  std::string punctuation;
  explicit PunctuationNode(const std::string& punctuation)
      : punctuation(punctuation) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "Punctuation: " << punctuation << "\n";
  }
};

// Operand Size Node
class OperandSizeNode : public ASTNode {
 public:
  std::string size;
  explicit OperandSizeNode(const std::string& size) : size(size) {}
  void print(int indent = 0) const override {
    printIndent(indent);
    std::cout << "Operand Size: " << size << "\n";
  }
};

#endif // AST_H
