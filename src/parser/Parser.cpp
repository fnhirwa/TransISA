#include "parser/Parser.h"
#include <unordered_set>

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

std::unordered_map<std::string, std::vector<BasicBlockNode*>>
    Parser::parserLabelMap;
std::unordered_map<std::string, FunctionNode*> Parser::parserFunctionMap;

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
        std::cerr << "Warning: Implicit .text section assumed: " << "At line "
                  << currentToken.line << ", column " << currentToken.column
                  << "\n";
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

static std::unordered_set<std::string>
    calledFunctions; // Track called functions

void Parser::parseTextSection(std::unique_ptr<RootNode>& root) {
  std::unique_ptr<FunctionNode> currentFunction = nullptr;
  std::unique_ptr<BasicBlockNode> currentBasicBlock = nullptr;

  while (index < tokens.size()) {
    Token currentToken = peek();

    if (currentToken.type == TokenType::SEGMENT_DIRECTIVE) {
      break;
    }

    if (currentToken.type == TokenType::LABEL) {
      std::string labelName = currentToken.value;

      // Only treat as a function if it's called or it's the entry point
      bool isFunction =
          (labelName == "_start" || calledFunctions.count(labelName) > 0);

      if (isFunction) {
        if (parserFunctionMap.find(labelName) == parserFunctionMap.end()) {
          if (currentFunction) {
            if (currentBasicBlock) {
              currentFunction->addBasicBlock(std::move(currentBasicBlock));
            }
            root->addBasicBlock(std::move(currentFunction));
          }

          currentFunction = std::make_unique<FunctionNode>(labelName);
          parserFunctionMap[labelName] = currentFunction.get();
        }
      }

      // Create a new BasicBlock for the label regardless of whether it's a
      // function
      if (currentBasicBlock) {
        if (currentFunction) {
          currentFunction->addBasicBlock(std::move(currentBasicBlock));
        }
      }
      currentBasicBlock = std::make_unique<BasicBlockNode>(labelName);
      parserLabelMap[currentFunction->name].push_back(currentBasicBlock.get());
      consume();
    }

    else if (currentToken.type == TokenType::INSTRUCTION) {
      std::string opcode = consume().value;
      std::vector<std::unique_ptr<ASTNode>> operands;

      while (peek().type != TokenType::INSTRUCTION &&
             peek().type != TokenType::LABEL &&
             peek().type != TokenType::SEGMENT_DIRECTIVE &&
             peek().type != TokenType::END) {
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
          continue;
        } else if (
            operandToken.value == "byte" || operandToken.value == "word" ||
            operandToken.value == "dword" || operandToken.value == "qword") {
          operands.push_back(
              std::make_unique<OperandSizeNode>(operandToken.value));
        } else if (operandToken.type == TokenType::IDENTIFIER) {
          operands.push_back(
              std::make_unique<MemoryNode>(operandToken.value, "", false));
        } else if (operandToken.type == TokenType::PUNCTUATION) {
          operands.push_back(
              std::make_unique<PunctuationNode>(operandToken.value));
        }
      }

      if (opcode == "call") {
        // Track called function name
        if (!operands.empty()) {
          auto* memoryNode = dynamic_cast<MemoryNode*>(operands[0].get());
          if (memoryNode) {
            calledFunctions.insert(memoryNode->base); // Mark as a function
          }
        }
      }

      auto instr =
          std::make_unique<InstructionNode>(opcode, std::move(operands));
      currentBasicBlock->addBasicBlock(std::move(instr));
    }

    else if (currentToken.type == TokenType::END) {
      break;
    }

    else {
      consume();
    }
  }

  if (currentFunction) {
    if (currentBasicBlock) {
      currentFunction->addBasicBlock(std::move(currentBasicBlock));
    }
    root->addBasicBlock(std::move(currentFunction));
  }
}

void Parser::parseGlobal(std::unique_ptr<RootNode>& root) {
  std::string varName = consume().value;
  auto varNode = std::make_unique<GlobalNode>(varName);
  root->addBasicBlock(std::move(varNode)); // Store in AST
}