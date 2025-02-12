#include <iostream>
#include "lexer/Lexer.h"

using namespace std;

int main() {
  std::string input = "mov eax, 5\n add ebx, 10\n";
  Lexer lexer(input);
  std::vector<Token> tokens = lexer.tokenize();

  for (const Token& token : tokens) {
    std::cout << "Token: " << token.value << " Type: " << token.type_name
              << "\n";
  }
  return 0;
}
