#include <gtest/gtest.h>
#include <cctype>
#include <iostream>
#include "lexer/Lexer.h"

using namespace std;

TEST(LexerTest, HandlesSimpleInput) {
  string test_example1 = ".section .rodata\nmov eax, 5\n ADd ebx, 10\n end";
  vector<Token> exepected_tokens = {
      {TokenType::SEGMENT_DIRECTIVE, ".section", "SEGMENT_DIRECTIVE", 1, 9},
      {TokenType::SEGMENT_DIRECTIVE, ".rodata", "SEGMENT_DIRECTIVE", 1, 17},
      {TokenType::INSTRUCTION, "mov", "INSTRUCTION", 2, 5},
      {TokenType::REGISTER, "eax", "REGISTER", 2, 9},
      {TokenType::COMMA, ",", "COMMA", 2, 10},
      {TokenType::IMMEDIATE, "5", "IMMEDIATE", 2, 12},
      {TokenType::INSTRUCTION, "add", "INSTRUCTION", 3, 6},
      {TokenType::REGISTER, "ebx", "REGISTER", 3, 10},
      {TokenType::COMMA, ",", "COMMA", 3, 11},
      {TokenType::IMMEDIATE, "10", "IMMEDIATE", 3, 14},
      {TokenType::INSTRUCTION, "end", "INSTRUCTION", 4, 6},
      {TokenType::END, "END", "END", 4, 6}};
  Lexer lexer(test_example1);
  vector<Token> tokens = lexer.tokenize();

  ASSERT_EQ(tokens.size(), exepected_tokens.size());
  // {TokenType::LABEL, value, "LABEL", line, column};
  for (int i = 0; i < exepected_tokens.size(); i++) {
    ASSERT_EQ(tokens[i].value, exepected_tokens[i].value);
    ASSERT_EQ(tokens[i].type_name, exepected_tokens[i].type_name);
    ASSERT_EQ(tokens[i].line, exepected_tokens[i].line);
    ASSERT_EQ(tokens[i].column, exepected_tokens[i].column);
    ASSERT_EQ(tokens[i].type, exepected_tokens[i].type);
  }
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}