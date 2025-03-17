#include <iostream>
#include <vector>
#include "lexer/Lexer.h"
#include "parser/Parser.h"

using namespace std;

void testParser(std::string_view source) {
  Lexer lexer(source);
  std::vector<Token> tokens = lexer.tokenize();
  // std::vector<Token> tokens = {
  //     Token{TokenType::SEGMENT_DIRECTIVE, ".text", "SEGMENT_DIRECTIVE", 1,
  //     1}, Token{TokenType::DIRECTIVE, "global", "DIRECTIVE", 2, 1}, // global
  //     _start Token{TokenType::IDENTIFIER, "_start", "IDENTIFIER", 2, 8},
  //     Token{TokenType::LABEL, "_start", "LABEL", 3, 1},
  //     Token{TokenType::INSTRUCTION, "mov", "INSTRUCTION", 4, 1},
  //     Token{TokenType::REGISTER, "eax", "REGISTER", 4, 5},
  //     Token{TokenType::COMMA, ",", "COMMA", 4, 8},
  //     Token{TokenType::IMMEDIATE, "4", "IMMEDIATE", 4, 10},
  //     Token{TokenType::INSTRUCTION, "int", "INSTRUCTION", 5, 1},
  //     Token{TokenType::HEX_IMMEDIATE, "80h", "HEX_IMMEDIATE", 5, 5},
  //     Token{TokenType::END, "", "END", 6, 1}};

  // {TokenType::LABEL, value, "LABEL", line, column};
  for (const Token& token : tokens) {
    std::cout << token.value << " " << token.type_name << " " << token.line
              << " " << token.column << std::endl;
  }
  // Parsing to AST
  Parser parser(tokens);
  std::unique_ptr<ASTNode> ast = parser.parse();

  std::cout << "Generated AST:\n";
  ast->print();
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <file-path>" << std::endl;
    return 1;
  }
  std::string filePath = argv[1];
  std::string bufferStorage;
  try {
    std::string_view source = readFileToStringView(filePath, bufferStorage);
    testParser(source);
  } catch (const std::exception& e) {
    std::cerr << e.what() << '\n';
  }

  return 0;
}
