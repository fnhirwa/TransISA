#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <vector>
#include "lexer/Token.h"
#include "lexer/Trie.h"

class Lexer {
 public:
  explicit Lexer(const std::string& source);
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

#endif // LEXER_H
