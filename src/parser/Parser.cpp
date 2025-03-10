#include "parser/Parser.h"

Token Parser::peek() {
  if (position < tokens.size())
    return tokens[position];
  return {TokenType::END, "", "END", 0, 0};
}

Token Parser::advance() {
  if (position < tokens.size())
    return tokens[position++];
  return {TokenType::END, "", "END", 0, 0};
}

bool Parser::match(TokenType type) {
  if (peek().type == type) {
    advance();
    return true;
  }
  return false;
}

std::unique_ptr<ASTNode> Parser::parseImmediate() {
  Token imm = advance();
  return std::make_unique<IntLiteralNode>(std::stoi(imm.value));
}

std::unique_ptr<ASTNode> Parser::parseRegister() {
  Token reg = advance();
  return std::make_unique<RegisterNode>(reg.value);
}

std::unique_ptr<ASTNode> Parser::parseLabel() {
  Token label = advance();
  return std::make_unique<LabelNode>(label.value);
}

std::unique_ptr<ASTNode> Parser::parseDirective() {
  Token directive = advance();
  return std::make_unique<DirectiveNode>(directive.value);
}

std::unique_ptr<ASTNode> Parser::parseString() {
  Token str = advance();
  return std::make_unique<StringNode>(str.value);
}

std::unique_ptr<ASTNode> Parser::parseInstruction() {
  Token instr = advance(); // Get instruction token (e.g., `mov`, `add`)
  std::vector<std::string> binaryInstructions = {
      "mov", "add", "sub", "mul", "div", "mod"};
  // hashmap for the instructions +, -, *, /, %
  std::unordered_map<std::string, std::string> binaryInstructionsMap = {
      {"add", "+"},
      {"sub", "-"},
      {"mul", "*"},
      {"div", "/"},
      {"mod", "%"},
      {"mov", "mov"}};
  // Check if the instruction is a binary operation
  if (std::find(
          binaryInstructions.begin(), binaryInstructions.end(), instr.value) !=
      binaryInstructions.end()) {
    // Parse the first operand
    auto dest = parseRegister();
    if (!dest)
      return nullptr;
    // skip the comma
    match(TokenType::PUNCTUATION);
    // Parse the second operand
    if (peek().type == TokenType::IMMEDIATE) {
      auto src = parseImmediate();
      if (!src)
        return nullptr;

      return std::make_unique<BinaryOpNode>(
          binaryInstructionsMap[instr.value], std::move(dest), std::move(src));
    } else if (peek().type == TokenType::REGISTER) {
      auto src = parseRegister();
      if (!src)
        return nullptr;
      return std::make_unique<BinaryOpNode>(
          binaryInstructionsMap[instr.value], std::move(dest), std::move(src));
    } else if (peek().type == TokenType::VALUE) {
      auto src = parseLabel();
      if (!src)
        return nullptr;
      return std::make_unique<BinaryOpNode>(
          binaryInstructionsMap[instr.value], std::move(dest), std::move(src));
    }
  }
  return nullptr;
}

// Function to parse a primary expression an expression that is not a binary
// operation. This can be an integer literal or a function call.
std::unique_ptr<ASTNode> Parser::parseExpression() {
  auto LHS = parsePrimaryExpr();
  if (!LHS)
    return nullptr;

  return parseBinaryOpRHS(0, std::move(LHS));
}

// // Function to parse function calls
std::unique_ptr<ASTNode> Parser::parseFunctionCall(const std::string& callee) {
  std::vector<std::unique_ptr<ASTNode>> args;
  if (position >= tokens.size() || tokens[position].value != "(")
    return nullptr;
  while (position < tokens.size() && tokens[position].value != ")") {
    args.push_back(parseExpression());
    if (position < tokens.size() && tokens[position].value == ",")
      position++;
  }
  if (position < tokens.size() && tokens[position].value == ")")
    position++;
  return std::make_unique<FunctionCallNode>(callee, std::move(args));
}

// // Function to parse binary operations
std::unique_ptr<ASTNode> Parser::parseBinaryOpRHS(
    int exprPrecedence,
    std::unique_ptr<ASTNode> LHS) {
  while (true) {
    if (position >= tokens.size())
      return LHS;
    Token currentToken = tokens[position];
    if (currentToken.type != TokenType::INSTRUCTION)
      return LHS;
    int currentPrecedence = getPrecedence(currentToken.value);
    if (currentPrecedence < exprPrecedence)
      return LHS;
    position++;
    auto RHS = parsePrimaryExpr();
    if (!RHS)
      return nullptr;
    if (position >= tokens.size())
      return std::make_unique<BinaryOpNode>(
          currentToken.value, std::move(LHS), std::move(RHS));
    Token nextToken = tokens[position];
    int nextPrecedence = getPrecedence(nextToken.value);
    if (currentPrecedence < nextPrecedence) {
      RHS = parseBinaryOpRHS(currentPrecedence + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }
    LHS = std::make_unique<BinaryOpNode>(
        currentToken.value, std::move(LHS), std::move(RHS));
  }
}