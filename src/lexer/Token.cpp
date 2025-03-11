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
    case TokenType::HEX_IMMEDIATE:
      type_name = "HEX_IMMEDIATE";
      break;
    case TokenType::BIN_IMMEDIATE:
      type_name = "BIN_IMMEDIATE";
      break;
    case TokenType::OCT_IMMEDIATE:
      type_name = "OCT_IMMEDIATE";
      break;
    case TokenType::LABEL:
      type_name = "LABEL";
      break;
    case TokenType::IDENTIFIER:
      type_name = "IDENTIFIER";
      break;
    case TokenType::DATA_VALUE:
      type_name = "DATA_VALUE";
      break;
    case TokenType::COMMA:
      type_name = "COMMA";
      break;
    case TokenType::COLON:
      type_name = "COLON";
      break;
    case TokenType::L_BRACKET:
      type_name = "L_BRACKET";
      break;
    case TokenType::R_BRACKET:
      type_name = "R_BRACKET";
      break;
    case TokenType::SEGMENT_DIRECTIVE:
      type_name = "SEGMENT_DIRECTIVE";
      break;
    case TokenType::PUNCTUATION:
      type_name = "PUNCTUATION";
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
    case TokenType::END:
      type_name = "END";
      break;
    default:
      type_name = "VALUE";
      break;
  }
  return type_name;
}