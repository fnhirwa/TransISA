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
  ASTNode* parse();

 private:
  std::vector<Token> tokens;
  size_t position = 0;

  Token peek();
  Token advance();
  bool match(TokenType type);
  ASTNode* parseInstruction();
  ASTNode* parseRegister();
  ASTNode* parseImmediate();
  ASTNode* parseOperand();
};

#endif // PARSER_H
