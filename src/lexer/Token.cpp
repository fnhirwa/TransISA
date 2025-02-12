#include "lexer/Token.h"

std::string getTokenTypeName(TokenType tok) {
  switch (tok) {
    case TokenType::INSTRUCTION:
      return "INSTRUCTION";
    case TokenType::REGISTER:
      return "REGISTER";
    case TokenType::IMMEDIATE:
      return "IMMEDIATE";
    case TokenType::PUNCTUATION:
      return "PUNCTUATION";
    case TokenType::LABEL:
      return "LABEL";
    case TokenType::END_OF_FILE:
      return "END_OF_FILE";
  }
  return "UNKNOWN";
}