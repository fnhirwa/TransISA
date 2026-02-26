#ifndef LEXER_H
#define LEXER_H

#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>
#include "lexer/Token.h"
#include "lexer/Trie.h"

/**
 * @brief Syntax mode detected or forced for the input file.
 */
enum class SyntaxMode { Intel, ATT, Auto };

/**
 * @brief Lexer class responsible for tokenizing x86 assembly source code.
 */

class Lexer {
 public:
  explicit Lexer(const std::string_view& source);
  std::vector<Token> tokenize();
  void reportError(const std::string& message);

  // Expose detected mode so callers can log/debug if needed
  SyntaxMode detectedMode() const {
    return detectedMode_;
  }

 private:
  std::string preprocessedSource_; // owns the normalized text
  std::string_view source; // points into preprocessedSource_
  size_t position = 0;
  size_t line = 1;
  size_t column = 0;
  size_t tokenStartCol_ = 1;
  size_t tokenStartLine_ = 1;
  SyntaxMode detectedMode_ = SyntaxMode::Intel;

  std::unique_ptr<Trie> keywordTrie;

  /**
   * @brief Preprocesses the raw x86 assembly source before lexical analysis.
   *
   * Responsibilities:
   * 1. Detect the syntax mode (AT&T vs Intel) from .intel_syntax / .att_syntax
   *     directives or heuristic scanning of the source code.
   * 2. If AT&T: Normalize to Intel syntax for consistent tokenization
   * (optional, based on design choice).
   *     - Strip `%` from register names: %eax -> eax
   *     - Strip `$` from immediate values: $1 -> 1
   *     - Convert memory operand syntax: -4(%rbp) -> [rbp-4]
   *     - Reverse operand order: movl src, dst -> mov dst, src
   *     - Normalize `#` line comments to `;`
   *     - Normalize mnemonic size suffixes: movl -> mov, addq -> add
   * 3. Strip semantically irrelevant directives (.cfi_*, .loc, .file, .ident)
   *     to make the parser's work easy.
   * 4. Return a clean std::string the lexer can consume.
   */
  std::string preprocess(const std::string& raw);
  SyntaxMode detectSyntax(const std::string& raw);
  std::string processLine(const std::string& line, bool isATT);
  std::string stripSizeSuffix(const std::string& mnemonic);
  std::string convertMemoryOperand(const std::string& op);
  std::string normalizeOperandList(
      const std::string& mnemonic,
      const std::string& operands);
  bool isDiscardableDirective(const std::string& line);

  /**
   * @brief Lexing helper methods for tokenizing the input string.
   * These methods read specific token types from the input string and return a
   * Token object. They are called by the main tokenize() method as it processes
   * the input.
   */
  char peek();
  char advance();
  void skipWhitespace();
  void skipComment();
  Token readInstruction();
  Token readRegister();
  Token readImmediate();
  Token readLabel();
  Token readIdentifier();
  Token readDirective();
  Token readValue();
  Token readComma();
  Token readColon();
  Token readLeftBracket();
  Token readRightBracket();
  Token readPunctuation();
  Token readString();
};

std::string tokenValueToLower(std::string_view str);
std::string readFileToString(const std::string& filePath);
std::string_view readFileToStringView(
    const std::string& filePath,
    std::string& bufferStorage);
bool isValidFileExtension(const std::string& filePath);
std::string detectNumberType(const std::string& value);

#endif // LEXER_H
