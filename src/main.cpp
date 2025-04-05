#include <iostream>
#include <vector>
#include "codegen/Codegen.h"
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
  // print the parsed labels
  std::cout << "Parsed Functions:\n";
  for (const auto& function : Parser::parserLabelMap) {
    std::cout << "Function: " << function.first << "\n";
    std::cout << "  Basic Blocks (" << function.second.size() << "):\n";
    std::cout << "Associated Labels: ";
    for (size_t i = 0; i < function.second.size(); ++i) {
      if (function.second[i] != nullptr) {
        std::cout << "    [" << i << "] " << function.second[i]->label << "\n";
        // Add more BasicBlockNode fields if needed
      } else {
        std::cout << "    [" << i << "] nullptr\n";
      }
    }
    std::cout << "\n";
  }
  // print the parsed functions
  std::cout << "Parsed Functions:\n";
  for (const auto& func : Parser::parserFunctionMap) {
    std::cout << "Function: " << func.first << "\n";
  }
  LLVMIRGen irGen;
  llvm::Module* module = irGen.generateIR(root);
  cout << "Generated LLVM IR:\n";
  module->print(llvm::errs(), nullptr);

  // assembly generation
  Codegen codegen;
  std::string outputFilename = "output.s";
  codegen.generateAssembly(*module, outputFilename);
  std::cout << "Assembly code generated in " << outputFilename << "\n";
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
