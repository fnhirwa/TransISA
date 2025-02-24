// The TokenType enum class is used to represent the type of token that the
// lexer has identified. The Token struct is used to store the token's value,
// type, and position in the input string. The getTokenTypeName function is used
// to convert the TokenType enum value to a string representation.
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
  END,
  DIRECTIVE,
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
