#include <iostream>
#include <vector>
#include "lexer/Lexer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm_ir/LLVMIRGenerator.h"
#include "parser/Parser.h"

using namespace std;

void testParser(std::string_view source) {
  Lexer lexer(source);
  std::vector<Token> tokens = lexer.tokenize();
  //{TokenType::LABEL, value, "LABEL", line, column};
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

void testIRGen(std::string_view source) {
  Lexer lexer(source);
  std::vector<Token> tokens = lexer.tokenize();
  for (const Token& token : tokens) {
    std::cout << token.value << " " << token.type_name << " " << token.line
              << " " << token.column << std::endl;
  }
  Parser parser(tokens);
  std::unique_ptr<ASTNode> root = parser.parse();
  std::cout << "Generated AST:\n";
  root->print();
  LLVMIRGen irGen;
  llvm::Module* module = irGen.generateIR(root);
  cout << "Generated LLVM IR:\n";
  module->print(llvm::errs(), nullptr);
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
    testIRGen(source);
  } catch (const std::exception& e) {
    std::cerr << e.what() << '\n';
  }

  return 0;
}
