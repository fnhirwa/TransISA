#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <vector>
#include "lexer/Token.h"
#include "lexer/Trie.h"

// The Lexer class is used to tokenize an input string. The tokenize method
// reads the input string character sequence and returns a vector of tokens. The
// reportError method is used to report an error with the lexer.
// The string_view is used to pass a reference to the input string without
// copying it. Using it can improve performance when passing large strings.
class Lexer {
 public:
  explicit Lexer(const std::string_view& source);
  std::vector<Token> tokenize();
  void reportError(const std::string& message);

 private:
  std::string_view source;
  size_t position = 0;
  size_t line = 1;
  size_t column = 1;

  std::unique_ptr<Trie> keywordTrie;

  char peek();
  char advance();
  void skipWhitespace();
  void skipComment();
  Token readInstruction();
  Token readKeyword();
  Token readRegister();
  Token readImmediate();
  Token readPunctuation();
  Token readLabel();
};

std::string tokenValueToLower(std::string_view str);

#endif // LEXER_H
