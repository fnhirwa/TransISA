#ifndef TRIE_H
#define TRIE_H

#include <lexer/Token.h>
#include <memory>
#include <string>
#include <unordered_map>

struct TrieNode {
  std::unordered_map<char, std::unique_ptr<TrieNode>> children;
  TokenType type = TokenType::INSTRUCTION;
  bool isEnd = false;
};

class Trie {
 public:
  Trie();
  void insert(const std::string& keyword, TokenType type);
  TokenType find(const std::string& keyword) const;

 private:
  std::unique_ptr<TrieNode> root;
};

#endif // TRIE_H
