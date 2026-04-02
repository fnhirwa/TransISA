#include <gtest/gtest.h>
#include <cctype>
#include <iostream>
#include "lexer/Lexer.h"
#include "parser/Parser.h"

using namespace std;

TEST(ParserTest, CreateAndWriteTempFile) {
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

  Parser parser(tokens);
  std::unique_ptr<ASTNode> ast = parser.parse();
  // check if the AST is not null
  ASSERT_TRUE(ast != nullptr) << "AST is null";
  // check if the AST is not empty
  ASSERT_TRUE(ast->children.size() > 0) << "AST is empty";
}

TEST(ParserTest, ParsesAddedOpsProgram) {
  const std::string src =
      "SECTION .text\n"
      "global _start\n"
      "_start:\n"
      "  mov eax, 7\n"
      "  not eax\n"
      "  imul eax, ebx\n"
      "  imul eax, ecx, 4\n"
      "  cdq\n"
      "  idiv ebx\n"
      "  movsx eax, ebx\n"
      "  movzx eax, ebx\n"
      "  xchg eax, ebx\n"
      "  ret\n";

  Lexer lexer(src);
  std::vector<Token> tokens = lexer.tokenize();

  Parser parser(tokens);
  std::unique_ptr<ASTNode> ast = parser.parse();

  ASSERT_TRUE(ast != nullptr) << "AST is null for added ops program";
  EXPECT_FALSE(ast->children.empty()) << "AST has no children";
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
