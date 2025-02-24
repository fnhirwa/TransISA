#include "lexer/Trie.h"

Trie::Trie() {
  root = std::make_unique<TrieNode>();
}

// Insert a keyword into the trie
void Trie::insert(const std::string& keyword, TokenType type) {
  TrieNode* node = root.get();
  for (char c : keyword) {
    if (!node->children[c]) {
      node->children[c] = std::make_unique<TrieNode>();
    }
    node = node->children[c].get();
  }
  node->isEnd = true;
  node->type = type;
}

// Find the token type for a given keyword
TokenType Trie::find(const std::string& keyword) const {
  TrieNode* node = root.get();
  for (char c : keyword) {
    if (!node->children.count(c))
      return TokenType::INVALID;
    node = node->children.at(c).get();
  }
  return node->isEnd ? node->type : TokenType::INVALID;
}