#ifndef AST_H
#define AST_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <memory>
#include <string>
#include <variant>
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
struct ASTNode {
  virtual ~ASTNode() = default;
  virtual llvm::Value* codegen(
      llvm::IRBuilder<>& builder,
      llvm::Module& module) = 0;
};

// Integer literals
struct IntLiteralNode : ASTNode {
  int value;
  IntLiteralNode(int value) : value(value) {}
  llvm::Value* codegen(llvm::IRBuilder<>& builder, llvm::Module& module)
      override {
    return llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(module.getContext()), value);
  }
};

// Register literals
struct RegisterNode : ASTNode {
  std::string value;
  RegisterNode(std::string value) : value(value) {}
  llvm::Value* codegen(llvm::IRBuilder<>& builder, llvm::Module& module)
      override {
    return nullptr;
  }
};

// Label Node
struct LabelNode : ASTNode {
  std::string value;
  LabelNode(std::string value) : value(value) {}
  llvm::Value* codegen(llvm::IRBuilder<>& builder, llvm::Module& module)
      override {
    return nullptr;
  }
};

// Directive Node
struct DirectiveNode : ASTNode {
  std::string value;
  DirectiveNode(std::string value) : value(value) {}
  llvm::Value* codegen(llvm::IRBuilder<>& builder, llvm::Module& module)
      override {
    return nullptr;
  }
};

// String Node
struct StringNode : ASTNode {
  std::string value;
  StringNode(std::string value) : value(value) {}
  llvm::Value* codegen(llvm::IRBuilder<>& builder, llvm::Module& module)
      override {
    return nullptr;
  }
};

// Node for binary operations (e.g., add, sub)
struct BinaryOpNode : ASTNode {
  std::string operation;
  std::unique_ptr<ASTNode> left_val, right_val;

  BinaryOpNode(
      std::string operation,
      std::unique_ptr<ASTNode> left_val,
      std::unique_ptr<ASTNode> right_val)
      : operation(operation), left_val(std::move(left_val)),
        right_val(std::move(right_val)) {}

  llvm::Value* codegen(llvm::IRBuilder<>& builder, llvm::Module& module)
      override {
    llvm::Value* L = left_val->codegen(builder, module);
    llvm::Value* R = right_val->codegen(builder, module);
    if (!L || !R)
      return nullptr;
    if (operation == "+")
      return builder.CreateAdd(L, R, "addtmp");
    if (operation == "-")
      return builder.CreateSub(L, R, "subtmp");
    if (operation == "*")
      return builder.CreateMul(L, R, "multmp");
    if (operation == "/")
      return builder.CreateSDiv(L, R, "divtmp");
    return nullptr;
  }
};

// Function calls Node
struct FunctionCallNode : ASTNode {
  std::string callee;
  std::vector<std::unique_ptr<ASTNode>> args;

  FunctionCallNode(
      std::string callee,
      std::vector<std::unique_ptr<ASTNode>> args)
      : callee(callee), args(std::move(args)) {}

  llvm::Value* codegen(llvm::IRBuilder<>& builder, llvm::Module& module)
      override {
    llvm::Function* func = module.getFunction(callee);
    if (!func) {
      throw std::runtime_error("Function not found: " + callee);
    }
    std::vector<llvm::Value*> argsValues;
    for (auto& arg : args) {
      argsValues.push_back(arg->codegen(builder, module));
    }
    return builder.CreateCall(func, argsValues, "calltmp");
  }
};

#endif // AST_H
