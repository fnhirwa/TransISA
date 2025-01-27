#ifndef IRNODE_H
#define IRNODE_H

#include <string>
#include <vector>

struct IRNode{
    virtual ~IRNode() = default;
};

struct IRInstruction : public IRNode {
    std::string opcode;
    std::vector<IRNode*> operands;
};

struct IROperand : public IRNode {
    std::string name;
};

#endif // IRNODE_H
