#include "lexer/Token.h"

std::string getTokenTypeName(TokenType tok) {
  std::string type_name;
  switch (tok) {
    case TokenType::INSTRUCTION:
      type_name = "INSTRUCTION";
      break;
    case TokenType::REGISTER:
      type_name = "REGISTER";
      break;
    case TokenType::IMMEDIATE:
      type_name = "IMMEDIATE";
      break;
    case TokenType::PUNCTUATION:
      type_name = "PUNCTUATION";
      break;
    case TokenType::LABEL:
      type_name = "LABEL";
      break;
    case TokenType::INVALID:
      type_name = "INVALID";
      break;
    case TokenType::DIRECTIVE:
      type_name = "DIRECTIVE";
      break;
    case TokenType::STRING:
      type_name = "STRING";
      break;
    case TokenType::VALUE:
      type_name = "VALUE";
      break;
    case TokenType::END:
      type_name = "END";
      break;
    default:
      type_name = "UNKNOWN";
      break;
  }
  return type_name;
}