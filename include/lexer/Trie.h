#ifndef TRIE_H
#define TRIE_H

#include <lexer/Token.h>
#include <memory>
#include <string>
#include <unordered_map>

// The TrieNode struct is used to represent a node in the trie. Each node has a
// map of characters to child nodes, a type, and a flag to indicate if it is the
// end of a keyword.
struct TrieNode {
  std::unordered_map<char, std::unique_ptr<TrieNode>> children;
  TokenType type = TokenType::INSTRUCTION;
  bool isEnd = false;
};

// The Trie class is used to store keywords and their corresponding token types
// in a trie data structure. The insert method is used to add a keyword to the
// trie, and the find method is used to look up the token type for a given
// keyword.
class Trie {
 public:
  Trie();
  void insert(const std::string& keyword, TokenType type);
  TokenType find(const std::string& keyword) const;

 private:
  std::unique_ptr<TrieNode> root;
};

#endif // TRIE_H
