#include "parser/Parser.h"

// Constructor: Initializes the parser with tokens
Parser::Parser(std::vector<Token> tokens)
    : tokens(std::move(tokens)), index(0) {}

// Peek at the current token without consuming
Token Parser::peek() {
  return (index < tokens.size()) ? tokens[index]
                                 : Token{TokenType::END, "END", "END", 0, 0};
}

// Consume the current token and advance
Token Parser::consume() {
  return (index < tokens.size()) ? tokens[index++]
                                 : Token{TokenType::END, "END", "END", 0, 0};
}

// Entry point for parsing
std::unique_ptr<RootNode> Parser::parse() {
  auto root = std::make_unique<RootNode>();

  while (index < tokens.size()) {
    Token currentToken = peek();
    if (currentToken.type == TokenType::SEGMENT_DIRECTIVE &&
        currentToken.value == ".data") {
      consume();
      parseDataSection(root);
    } else if (
        currentToken.type == TokenType::SEGMENT_DIRECTIVE &&
        currentToken.value == ".text") {
      consume();
      parseTextSection(root);
    } else {
      consume(); // Skip unrecognized tokens
    }
  }
  return root;
}

// Parse the .data section (global variables)
void Parser::parseDataSection(std::unique_ptr<RootNode>& root) {
  while (index < tokens.size()) {
    Token currentToken = peek();
    if (currentToken.type == TokenType::SEGMENT_DIRECTIVE)
      break; // End of .data section

    if (currentToken.type == TokenType::IDENTIFIER) {
      std::string varName = consume().value;
      if (peek().type == TokenType::DATA_VALUE) {
        consume(); // Consume db, dw, etc.
        Token valueToken = consume();
        auto varNode =
            std::make_unique<GlobalVariableNode>(varName, valueToken.value);
        root->addBasicBlock(std::move(varNode)); // Store in AST
      }
    } else {
      consume(); // Ignore other tokens
    }
  }
}

// Parse the .text section (functions & instructions)
void Parser::parseTextSection(std::unique_ptr<RootNode>& root) {
  std::unique_ptr<FunctionNode> function = nullptr;
  std::unique_ptr<BasicBlockNode> basicBlock = nullptr;

  while (index < tokens.size()) {
    Token currentToken = peek();
    if (currentToken.type == TokenType::SEGMENT_DIRECTIVE)
      break; // End of .text section

    if (currentToken.type == TokenType::LABEL) {
      if (function) {
        function->addBasicBlock(std::move(basicBlock));
        root->addBasicBlock(std::move(function));
      }
      function = std::make_unique<FunctionNode>(currentToken.value);
      basicBlock = std::make_unique<BasicBlockNode>("entry");
      consume(); // Consume label
    } else if (currentToken.type == TokenType::INSTRUCTION) {
      std::string opcode = consume().value;
      std::vector<std::unique_ptr<ASTNode>> operands;

      while (peek().type != TokenType::INSTRUCTION &&
             peek().type != TokenType::LABEL && peek().type != TokenType::END) {
        Token operandToken = consume();
        std::string isNumber = detectNumberType(operandToken.value);
        if (operandToken.type == TokenType::REGISTER) {
          operands.push_back(
              std::make_unique<RegisterNode>(operandToken.value));
        } else if (isNumber == "Decimal") {
          operands.push_back(std::make_unique<IntLiteralNode>(
              std::stoi(operandToken.value, nullptr, 10)));
        } else if (isNumber == "Hexadecimal") {
          operands.push_back(std::make_unique<IntLiteralNode>(
              std::stoi(operandToken.value, nullptr, 16)));
        } else if (operandToken.type == TokenType::STRING) {
          operands.push_back(std::make_unique<StringNode>(operandToken.value));
        } else if (operandToken.type == TokenType::COMMA) {
          continue; // Skip commas
        } else if (operandToken.type == TokenType::L_BRACKET) {
          // we have indirect memory access
          std::string base =
              consume().value; // base register or memory location
          std::string offset = "";
          bool isIndirect = true;
          if (peek().value == "+") {
            consume(); // consume '+'
            offset = consume().value; // offset
          }
          consume(); // consume ']'
          operands.push_back(
              std::make_unique<MemoryNode>(base, offset, isIndirect));
        } else {
          consume();
        }
      }
      auto instr =
          std::make_unique<InstructionNode>(opcode, std::move(operands));
      basicBlock->addBasicBlock(std::move(instr));
    } else {
      consume();
    }
  }

  if (function) {
    function->addBasicBlock(std::move(basicBlock));
    root->addBasicBlock(std::move(function));
  }
}
