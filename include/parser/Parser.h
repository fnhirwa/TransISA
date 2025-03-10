#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include "lexer/Token.h"
#include "parser/AST.h"

// The Parser class is used to parse a sequence of tokens into an abstract
// syntax tree (AST). The parse method reads the tokens and constructs an AST
// based on the grammar rules of the language.

class Parser {
 public:
  explicit Parser(const std::vector<Token>& tokens);
  std::unique_ptr<ASTNode> parseExpression();

 private:
  std::vector<Token> tokens;
  size_t position = 0;
  // for passing custom tokens from the lexer
  Token peek();
  Token advance();
  bool match(TokenType type);
  std::unique_ptr<ASTNode> parseImmediate();
  std::unique_ptr<ASTNode> parseRegister();
  std::unique_ptr<ASTNode> parseLabel();
  std::unique_ptr<ASTNode> parseDirective();
  std::unique_ptr<ASTNode> parseString();
  std::unique_ptr<ASTNode> parseKeyword();
  std::unique_ptr<ASTNode> parseInstruction();

  // for parsing expressions
  std::unique_ptr<ASTNode> parsePrimaryExpr(); // for integer literals
  std::unique_ptr<ASTNode> parseFunctionCall(
      const std::string& callee); // for function calls
  std::unique_ptr<ASTNode> parseBinaryOpRHS(
      int exprPrecedence,
      std::unique_ptr<ASTNode> LHS); // for binary operations

  int getPrecedence(const std::string& op) {
    if (op == "+" || op == "-")
      return 10;
    if (op == "*" || op == "/")
      return 20;
    return 0;
  }
};

#endif // PARSER_H
