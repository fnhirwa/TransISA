#ifndef AST_H
#define AST_H

#include <string>
#include <vector>

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

#endif // AST_H
