#include "parser/Parser.h"

// Constructor: Initializes the parser with tokens
Parser::Parser(std::vector<Token> tokens)
    : tokens(std::move(tokens)), index(0) {}

// Peek at the current token without consuming
Token Parser::peek() {
  return (index < tokens.size()) ? tokens[index]
                                 : Token{
                                       TokenType::END,
                                       "END",
                                       "END",
                                       tokens[tokens.size() - 1].line + 1,
                                       0};
}

// Consume the current token and advance
Token Parser::consume() {
  return (index < tokens.size()) ? tokens[index++]
                                 : Token{
                                       TokenType::END,
                                       "END",
                                       "END",
                                       tokens[tokens.size() - 1].line + 1,
                                       0};
}

// Entry point for parsing
std::unique_ptr<RootNode> Parser::parse() {
  auto root = std::make_unique<RootNode>();
  bool inTextSection = false;

  while (index < tokens.size()) {
    Token currentToken = peek();

    if (currentToken.type == TokenType::END) {
      break; // End of tokens
    }

    // Global variables
    if (currentToken.type == TokenType::DIRECTIVE &&
        currentToken.value == "global") {
      consume(); // Consume 'global' keyword
      parseGlobal(root);
    }

    else if (currentToken.type == TokenType::SEGMENT_DIRECTIVE) {
      if (currentToken.value == "section" || currentToken.value == ".section")
        consume(); // Consume 'section' keyword
      else if (
          currentToken.value == ".data" || currentToken.value == ".rodata") {
        consume();
        parseDataSection(root);
      } else if (currentToken.value == ".bss") {
        consume();
        parseBssSection(root);
      } else if (currentToken.value == ".text") {
        consume();
        inTextSection = true;
        parseTextSection(root);
      }
      continue; // Ensure loop continues processing other sections
    }

    // If encountering a label or instruction before .text, assume an implicit
    // .text section
    else if (
        currentToken.type == TokenType::LABEL ||
        currentToken.type == TokenType::INSTRUCTION) {
      if (!inTextSection) {
        inTextSection = true;
        std::cerr << "Warning: Implicit .text section assumed\n";
        parseTextSection(root);
      } else {
        consume(); // Already inside .text, just continue parsing
      }
      continue; // Ensure loop keeps processing other sections
    }

    else {
      consume(); // Skip unknown tokens
    }
  }

  return root;
}

// Parse the .data section (global variables)
void Parser::parseDataSection(std::unique_ptr<RootNode>& root) {
  while (index < tokens.size()) {
    Token currentToken = peek();
    if (currentToken.type == TokenType::SEGMENT_DIRECTIVE ||
        currentToken.type == TokenType::LABEL ||
        currentToken.type == TokenType::INSTRUCTION)
      break; // End of .data section (or start of implicit .text)

    if (currentToken.type == TokenType::IDENTIFIER) {
      std::string varName = consume().value;

      if (peek().type == TokenType::DATA_VALUE) { // Handle db, dw, etc.
        consume(); // Consume type specifier
        Token valueToken = consume();
        auto varNode =
            std::make_unique<GlobalVariableNode>(varName, valueToken.value);
        root->addBasicBlock(std::move(varNode)); // Store in AST
      } else {
        std::cerr << "Error: Invalid data value in .data section\n";
        consume(); // Consume invalid token
      }
    } else {
      consume(); // Ignore other tokens
    }
  }
}

// Parse the .bss section (uninitialized global variables)
void Parser::parseBssSection(std::unique_ptr<RootNode>& root) {
  while (index < tokens.size()) {
    Token currentToken = peek();

    // Exit if a new section starts
    if (currentToken.type == TokenType::SEGMENT_DIRECTIVE ||
        currentToken.type == TokenType::LABEL ||
        currentToken.type == TokenType::INSTRUCTION) {
      break;
    }

    // Parse variable definition
    if (currentToken.type == TokenType::IDENTIFIER) {
      std::string varName = consume().value; // Get variable name

      // Next token should be a reserve directive (resb, resw, etc.)
      if (peek().type == TokenType::DIRECTIVE &&
          (peek().value == "resb" ||
           peek().value == "resw")) { // FIX: Ensure correct type
        std::string directive = consume().value;

        // Next token should be the size
        if (peek().type ==
            TokenType::IMMEDIATE) { // FIX: Ensure it's an immediate value
          std::string size = consume().value;
          auto varNode = std::make_unique<BssNode>(varName, directive, size);
          root->addBasicBlock(std::move(varNode));
        } else {
          std::cerr << "Error: Missing size for " << directive
                    << " directive\n";
        }
      } else {
        std::cerr << "Error: Expected reserve directive after variable name\n";
        consume(); // Skip the invalid token
      }
    } else {
      consume(); // Skip unexpected tokens
    }
  }
}

// Parse the .text section (functions & instructions)
void Parser::parseTextSection(std::unique_ptr<RootNode>& root) {
  std::unique_ptr<FunctionNode> function = nullptr;
  std::unique_ptr<BasicBlockNode> basicBlock = nullptr;

  while (index < tokens.size()) {
    Token currentToken = peek();
    std::cerr << "Next token is: " << currentToken.value << " of type "
              << currentToken.type_name << " at line " << currentToken.line
              << "\n";

    // **Check for a new section and exit immediately**
    if (currentToken.type == TokenType::SEGMENT_DIRECTIVE) {
      std::cerr << "Detected new segment: " << currentToken.value << "\n";
      break; // Exit parsing the .text section
    }

    // Handle function labels
    if (currentToken.type == TokenType::LABEL) {
      if (function) {
        function->addBasicBlock(std::move(basicBlock));
        root->addBasicBlock(std::move(function)); // Finalize previous function
      }
      function = std::make_unique<FunctionNode>(currentToken.value);
      basicBlock = std::make_unique<BasicBlockNode>("entry");
      consume();
    }

    // Handle instructions
    else if (currentToken.type == TokenType::INSTRUCTION) {
      std::string opcode = consume().value;
      std::vector<std::unique_ptr<ASTNode>> operands;

      // Parse operands
      while (peek().type != TokenType::INSTRUCTION &&
             peek().type != TokenType::LABEL &&
             peek().type != TokenType::SEGMENT_DIRECTIVE &&
             peek().type != TokenType::END) { // **Check for new section**
        Token operandToken = consume();
        std::string numberType = detectNumberType(operandToken.value);

        if (operandToken.type == TokenType::REGISTER) {
          operands.push_back(
              std::make_unique<RegisterNode>(operandToken.value));
        } else if (numberType == "Decimal") {
          operands.push_back(std::make_unique<IntLiteralNode>(
              std::stoi(operandToken.value, nullptr, 10)));
        } else if (numberType == "Hexadecimal") {
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
          consume(); // Skip unknown tokens
        }
      }

      // Add instruction to the basic block
      auto instr =
          std::make_unique<InstructionNode>(opcode, std::move(operands));
      basicBlock->addBasicBlock(std::move(instr));
    }

    else if (currentToken.type == TokenType::END) {
      break; // End of tokens
    }

    else {
      consume(); // Skip unknown tokens
    }
  }

  // Finalize any remaining function
  if (function) {
    function->addBasicBlock(std::move(basicBlock));
    root->addBasicBlock(std::move(function));
  }
}

void Parser::parseGlobal(std::unique_ptr<RootNode>& root) {
  std::string varName = consume().value;
  auto varNode = std::make_unique<GlobalNode>(varName);
  root->addBasicBlock(std::move(varNode)); // Store in AST
}