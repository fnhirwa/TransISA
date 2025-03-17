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
      std::vector<std::string> operands;
      while (peek().type != TokenType::INSTRUCTION &&
             peek().type != TokenType::LABEL && peek().type != TokenType::END) {
        operands.push_back(consume().value);
      }
      auto instr = std::make_unique<InstructionNode>(opcode, operands);
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
