#include <iostream>
#include "lexer/Lexer.h"

using namespace std;

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <file-path>" << std::endl;
    return 1;
  }
  std::string filePath = argv[1];
  std::string bufferStorage;
  try {
    std::string_view source = readFileToStringView(filePath, bufferStorage);
    Lexer lexer(source);
    std::vector<Token> tokens = lexer.tokenize();
    // {TokenType::LABEL, value, "LABEL", line, column};
    for (const Token& token : tokens) {
      std::cout << token.value << " " << token.type_name << " " << token.line
                << " " << token.column << std::endl;
    }
  } catch (const std::exception& e) {
    std::cerr << e.what() << '\n';
  }
  return 0;
}
