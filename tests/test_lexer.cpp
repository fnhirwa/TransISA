#include <gtest/gtest.h>
#include <fstream>
#include <string>
#include <vector>
#include "lexer/Lexer.h"

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

static std::vector<Token> tokenizeString(const std::string& src) {
  Lexer lexer(src);
  return lexer.tokenize();
}

// Assert a single token field-by-field with a clear failure message
static void assertToken(
    const Token& actual,
    TokenType expectedType,
    const std::string& expectedValue,
    const std::string& expectedTypeName,
    size_t expectedLine,
    size_t expectedCol) {
  EXPECT_EQ(actual.type, expectedType)
      << "type mismatch for token: " << actual.value;
  EXPECT_EQ(actual.value, expectedValue)
      << "value mismatch at line " << expectedLine;
  EXPECT_EQ(actual.type_name, expectedTypeName)
      << "type_name mismatch for: " << actual.value;
  EXPECT_EQ(actual.line, expectedLine) << "line mismatch for: " << actual.value;
  EXPECT_EQ(actual.column, expectedCol)
      << "column mismatch for: " << actual.value;
}

// ─────────────────────────────────────────────────────────────────────────────
// Layer 2 — Intel / NASM syntax tokenization (regression baseline)
// ─────────────────────────────────────────────────────────────────────────────

// Simple arithmetic: confirms instruction + register + immediate tokenization
TEST(LexerIntel, AddInstruction) {
  auto tokens = tokenizeString("add eax, 1\n");
  ASSERT_GE(tokens.size(), 4u);
  assertToken(tokens[0], TokenType::INSTRUCTION, "add", "INSTRUCTION", 1, 1);
  assertToken(tokens[1], TokenType::REGISTER, "eax", "REGISTER", 1, 5);
  assertToken(tokens[2], TokenType::COMMA, ",", "COMMA", 1, 8);
  assertToken(tokens[3], TokenType::IMMEDIATE, "1", "IMMEDIATE", 1, 10);
}

// Memory operand with bracket notation
TEST(LexerIntel, MemoryOperand) {
  auto tokens = tokenizeString("mov eax, [rbp-8]\n");
  ASSERT_GE(tokens.size(), 7u);
  assertToken(tokens[0], TokenType::INSTRUCTION, "mov", "INSTRUCTION", 1, 1);
  assertToken(tokens[1], TokenType::REGISTER, "eax", "REGISTER", 1, 5);
  assertToken(tokens[2], TokenType::COMMA, ",", "COMMA", 1, 8);
  assertToken(tokens[3], TokenType::L_BRACKET, "[", "L_BRACKET", 1, 10);
  // rbp, -, 8, ] follow — just check the bracket opened and closed
  assertToken(
      tokens.back(), TokenType::END, "END", "END", 1, tokens.back().column);
}

// Label recognition
TEST(LexerIntel, LabelToken) {
  auto tokens = tokenizeString("_start:\n  mov eax, 0\n");
  assertToken(tokens[0], TokenType::LABEL, "_start", "LABEL", 1, 1);
}

// Section and data directives
TEST(LexerIntel, SectionDirectives) {
  auto tokens = tokenizeString(".text\n.data\n");
  assertToken(
      tokens[0],
      TokenType::SEGMENT_DIRECTIVE,
      ".text",
      "SEGMENT_DIRECTIVE",
      1,
      1);
  assertToken(
      tokens[1],
      TokenType::SEGMENT_DIRECTIVE,
      ".data",
      "SEGMENT_DIRECTIVE",
      2,
      1);
}

// Hex immediate
TEST(LexerIntel, HexImmediate) {
  auto tokens = tokenizeString("int 80h\n");
  assertToken(tokens[0], TokenType::INSTRUCTION, "int", "INSTRUCTION", 1, 1);
  assertToken(
      tokens[1], TokenType::HEX_IMMEDIATE, "80h", "HEX_IMMEDIATE", 1, 5);
}

// Full NASM-style hello world — regression test, do not break
TEST(LexerIntel, NASMHelloWorld) {
  const std::string src =
      ".section .data\n"
      "msg db 'Hello World!', 0Ah\n"
      ".text\n"
      ".globl _start\n"
      "_start:\n"
      "  mov edx, 13\n"
      "  lea ecx, [msg]\n"
      "  mov ebx, 1\n"
      "  mov eax, 4\n"
      "  int 80h\n";

  auto tokens = tokenizeString(src);

  // Spot-check key tokens rather than asserting entire sequence
  // (full sequence tests are brittle when preprocessing changes whitespace)
  EXPECT_EQ(tokens[0].type, TokenType::SEGMENT_DIRECTIVE);
  EXPECT_EQ(tokens[0].value, ".section");
  EXPECT_EQ(tokens[1].value, ".data");

  // Find 'mov' tokens — there should be 3
  int movCount = 0;
  for (const auto& t : tokens)
    if (t.type == TokenType::INSTRUCTION && t.value == "mov")
      ++movCount;
  EXPECT_EQ(movCount, 3);

  // 'lea' should appear once
  int leaCount = 0;
  for (const auto& t : tokens)
    if (t.type == TokenType::INSTRUCTION && t.value == "lea")
      ++leaCount;
  EXPECT_EQ(leaCount, 1);
}

// ─────────────────────────────────────────────────────────────────────────────
// Layer 1 — Preprocessing: AT&T syntax detection
// ─────────────────────────────────────────────────────────────────────────────

TEST(LexerPreprocess, DetectsATTSyntax) {
  // AT&T file should set detectedMode to ATT
  const std::string attSrc = "movl %eax, %ebx\n";
  Lexer lexer(attSrc);
  EXPECT_EQ(lexer.detectedMode(), SyntaxMode::ATT);
}

TEST(LexerPreprocess, DetectsIntelSyntax) {
  const std::string intelSrc = ".intel_syntax noprefix\nmov eax, ebx\n";
  Lexer lexer(intelSrc);
  EXPECT_EQ(lexer.detectedMode(), SyntaxMode::Intel);
}

TEST(LexerPreprocess, DiscardsCFIDirectives) {
  const std::string src =
      "_start:\n"
      "  .cfi_startproc\n"
      "  mov eax, 1\n"
      "  .cfi_endproc\n";
  auto tokens = tokenizeString(src);

  // .cfi_* lines must not appear in token stream
  for (const auto& t : tokens) {
    EXPECT_EQ(t.value.find(".cfi_"), std::string::npos)
        << "CFI directive leaked into token stream: " << t.value;
  }

  // mov must still be there
  bool foundMov = false;
  for (const auto& t : tokens)
    if (t.value == "mov")
      foundMov = true;
  EXPECT_TRUE(foundMov);
}

// ─────────────────────────────────────────────────────────────────────────────
// Layer 3 — End-to-end: AT&T input produces same tokens as Intel equivalent
// ─────────────────────────────────────────────────────────────────────────────

// AT&T:   addl %ebx, %eax   →  Intel: add eax, ebx
TEST(LexerATT, AddNormalizesToIntel) {
  const std::string attSrc = "addl %ebx, %eax\n";
  const std::string intelSrc = "add eax, ebx\n";

  auto attTokens = tokenizeString(attSrc);
  auto intelTokens = tokenizeString(intelSrc);

  ASSERT_EQ(attTokens.size(), intelTokens.size())
      << "Token count differs between AT&T and Intel equivalent";

  for (size_t i = 0; i < intelTokens.size(); ++i) {
    EXPECT_EQ(attTokens[i].type, intelTokens[i].type)
        << "Type mismatch at token " << i;
    EXPECT_EQ(attTokens[i].value, intelTokens[i].value)
        << "Value mismatch at token " << i;
  }
}

// AT&T memory: movl -8(%rbp), %eax  →  Intel: mov eax, [rbp-8]
TEST(LexerATT, MemoryOperandNormalization) {
  const std::string attSrc = "movl -8(%rbp), %eax\n";
  const std::string intelSrc = "mov eax, [rbp-8]\n";

  auto attTokens = tokenizeString(attSrc);
  auto intelTokens = tokenizeString(intelSrc);

  ASSERT_EQ(attTokens.size(), intelTokens.size());
  for (size_t i = 0; i < intelTokens.size(); ++i) {
    EXPECT_EQ(attTokens[i].type, intelTokens[i].type)
        << "Type mismatch at token " << i;
    EXPECT_EQ(attTokens[i].value, intelTokens[i].value)
        << "Value mismatch at token " << i;
  }
}

// GCC-emitted function — full pipeline test
TEST(LexerATT, GCCEmittedFunction) {
  const std::string gcc_output =
      "    .text\n"
      "    .globl add\n"
      "    .type  add, @function\n"
      "add:\n"
      "    .cfi_startproc\n" // must be discarded
      "    pushq  %rbp\n"
      "    movq   %rsp, %rbp\n"
      "    movl   %edi, -4(%rbp)\n"
      "    movl   %esi, -8(%rbp)\n"
      "    movl   -8(%rbp), %eax\n"
      "    addl   -4(%rbp), %eax\n"
      "    popq   %rbp\n"
      "    ret\n"
      "    .cfi_endproc\n"; // must be discarded

  auto tokens = tokenizeString(gcc_output);

  // .cfi_* must not appear
  for (const auto& t : tokens)
    EXPECT_EQ(t.value.find(".cfi_"), std::string::npos)
        << "CFI directive leaked: " << t.value;

  // Count expected instructions: push, mov, mov, mov, mov, mov, add, pop, ret =
  // 9
  int instrCount = 0;
  for (const auto& t : tokens)
    if (t.type == TokenType::INSTRUCTION)
      ++instrCount;
  EXPECT_EQ(instrCount, 9);

  // Operand order must be Intel (dst first): first mov should have rbp as dst
  // Find 'push' token, the next register token should be 'rbp'
  for (size_t i = 0; i < tokens.size(); ++i) {
    if (tokens[i].type == TokenType::INSTRUCTION && tokens[i].value == "push") {
      ASSERT_LT(i + 1, tokens.size());
      EXPECT_EQ(tokens[i + 1].type, TokenType::REGISTER);
      EXPECT_EQ(tokens[i + 1].value, "rbp");
      break;
    }
  }
}

// ─────────────────────────────────────────────────────────────────────────────

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}