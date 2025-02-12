#include <iostream>
#include "lexer/Lexer.h"

using namespace std;

int main() {
  std::string input = "mov eax, 5";
  Lexer lexer(input);
  std::vector<Token> tokens = lexer.tokenize();

  for (const Token& token : tokens) {
    std::cout << "Token: " << token.value
              << " Type: " << static_cast<int>(token.type) << "\n";
  }

  return 0;
}
