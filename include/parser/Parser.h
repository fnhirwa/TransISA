#ifndef PARSER_H
#define PARSER_H

#include <iostream>
#include <memory>
#include <vector>
#include "lexer/Lexer.h"
#include "lexer/Token.h"
#include "parser/AST.h"

// The Parser class is used to parse a sequence of tokens into an abstract
// syntax tree (AST). The parse method reads the tokens and constructs an AST
// based on the grammar rules of the language.
class Parser {
 private:
  std::vector<Token> tokens;
  size_t index;

  Token peek();
  Token consume();

 public:
  explicit Parser(std::vector<Token> tokens);
  std::unique_ptr<RootNode> parse();
  // label map to store the label and its corresponding basic block
  std::unordered_map<std::string, BasicBlockNode*> parserLabelMap;

 private:
  void parseDataSection(std::unique_ptr<RootNode>& root);
  void parseBssSection(std::unique_ptr<RootNode>& root);
  void parseTextSection(std::unique_ptr<RootNode>& root);
  void parseInstruction(std::unique_ptr<RootNode>& root);
  void parseLabel(std::unique_ptr<RootNode>& root, const Token& token);
  void parseGlobal(std::unique_ptr<RootNode>& root);
};

#endif // PARSER_H
