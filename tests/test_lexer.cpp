#include <gtest/gtest.h>
#include <cctype>
#include <iostream>
#include "lexer/Lexer.h"

using namespace std;

TEST(LexerTest, CreateAndWriteTempFile) {
  string test_example1 =
      ".section .data\nmsg db 'Hello World!', 0Ah\nSECTION .text\n global  "
      "_start\n_start:\n mov edx, 13\nlea ecx, [msg]\nmov ebx, 1\nmov eax, "
      "4\nint 80h\nend";
  vector<Token> exepected_tokens = {
      {TokenType::SEGMENT_DIRECTIVE, ".section", "SEGMENT_DIRECTIVE", 1, 9},
      {TokenType::SEGMENT_DIRECTIVE, ".data", "SEGMENT_DIRECTIVE", 1, 15},
      {TokenType::IDENTIFIER, "msg", "IDENTIFIER", 2, 4},
      {TokenType::DATA_VALUE, "db", "DATA_VALUE", 2, 7},
      {TokenType::STRING, "Hello World!", "STRING", 2, 22},
      {TokenType::COMMA, ",", "COMMA", 2, 23},
      {TokenType::HEX_IMMEDIATE, "0ah", "HEX_IMMEDIATE", 2, 27},
      {TokenType::SEGMENT_DIRECTIVE, "section", "SEGMENT_DIRECTIVE", 3, 8},
      {TokenType::SEGMENT_DIRECTIVE, ".text", "SEGMENT_DIRECTIVE", 3, 14},
      {TokenType::DIRECTIVE, "global", "DIRECTIVE", 4, 8},
      {TokenType::IDENTIFIER, "_start", "IDENTIFIER", 4, 16},
      {TokenType::LABEL, "_start", "LABEL", 5, 8},
      {TokenType::INSTRUCTION, "mov", "INSTRUCTION", 6, 5},
      {TokenType::REGISTER, "edx", "REGISTER", 6, 9},
      {TokenType::COMMA, ",", "COMMA", 6, 10},
      {TokenType::IMMEDIATE, "13", "IMMEDIATE", 6, 13},
      {TokenType::INSTRUCTION, "lea", "INSTRUCTION", 7, 4},
      {TokenType::REGISTER, "ecx", "REGISTER", 7, 8},
      {TokenType::COMMA, ",", "COMMA", 7, 9},
      {TokenType::L_BRACKET, "[", "L_BRACKET", 7, 11},
      {TokenType::IDENTIFIER, "msg", "IDENTIFIER", 7, 14},
      {TokenType::R_BRACKET, "]", "R_BRACKET", 7, 15},
      {TokenType::INSTRUCTION, "mov", "INSTRUCTION", 8, 4},
      {TokenType::REGISTER, "ebx", "REGISTER", 8, 8},
      {TokenType::COMMA, ",", "COMMA", 8, 9},
      {TokenType::IMMEDIATE, "1", "IMMEDIATE", 8, 11},
      {TokenType::INSTRUCTION, "mov", "INSTRUCTION", 9, 4},
      {TokenType::REGISTER, "eax", "REGISTER", 9, 8},
      {TokenType::COMMA, ",", "COMMA", 9, 9},
      {TokenType::IMMEDIATE, "4", "IMMEDIATE", 9, 11},
      {TokenType::INSTRUCTION, "int", "INSTRUCTION", 10, 4},
      {TokenType::HEX_IMMEDIATE, "80h", "HEX_IMMEDIATE", 10, 8},
      {TokenType::INSTRUCTION, "end", "INSTRUCTION", 11, 4},
      {TokenType::END, "END", "END", 11, 4}};
  const std::string temp_assembly_file = "example.s";
  // create a temporary assemby file
  std::ofstream out(temp_assembly_file);
  ASSERT_TRUE(out.is_open()) << "Could not create temporary assembly file";
  out << test_example1;
  out.close();
  // check if tbe file is readable
  std::ifstream in(temp_assembly_file);
  ASSERT_TRUE(in.is_open()) << "Could not open temporary assembly file";
  in.close();
  // read the file
  std::string bufferStorage;
  std::string_view assembly_input =
      readFileToStringView(temp_assembly_file, bufferStorage);

  Lexer lexer(assembly_input);
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