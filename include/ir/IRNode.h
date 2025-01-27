#ifndef IRNODE_H
#define IRNODE_H

#include <string>
#include <variant>
#include <vector>

struct IRNode {
  virtual ~IRNode() = default;
};

struct IRInstruction : public IRNode {
  std::string opcode;
  std::vector<IRNode*> operands;
};

struct IROperand : public IRNode {
  std::string name;
  std::variant<IRInstruction*, int, float> value;
};

#endif // IRNODE_H
