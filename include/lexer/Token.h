#ifndef TOKEN_H
#define TOKEN_H

#include <string>

enum class TokenType {
  INSTRUCTION,
  REGISTER,
  IMMEDIATE,
  PUNCTUATION,
  LABEL,
  INVALID,
  DIRECTIVE,
  END_OF_FILE
};

struct Token {
  TokenType type;
  std::string value;
  std::string type_name;
  size_t line;
  size_t column;
};

std::string getTokenTypeName(TokenType tok);

#endif // TOKEN_H
