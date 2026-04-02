#include <gtest/gtest.h>
#include <cctype>
#include <fstream>
#include <iostream>
#include <string>
#include "lexer/Lexer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm_ir/LLVMIRGenerator.h"
#include "parser/Parser.h"

using namespace std;

static llvm::Module* buildModuleFromSource(const std::string& src) {
  const std::string tmpFile = "gap1_test_tmp.s";

  std::ofstream out(tmpFile);
  if (!out.is_open())
    return nullptr;
  out << src;
  out.close();

  std::string bufferStorage;
  std::string_view view = readFileToStringView(tmpFile, bufferStorage);

  Lexer lexer(view);
  std::vector<Token> tokens = lexer.tokenize();

  Parser parser(tokens);
  std::unique_ptr<ASTNode> ast = parser.parse();

  static LLVMIRGen irGen;
  return irGen.generateIR(ast);
}

static std::string dumpIR(llvm::Module* mod) {
  std::string ir;
  llvm::raw_string_ostream rso(ir);
  mod->print(rso, nullptr);
  return ir;
}

TEST(IRGenTest, CreateAndWriteTempFile) {
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

  LLVMIRGen irGen;
  llvm::Module* module = irGen.generateIR(ast);
  // check if the llvm module was created
  ASSERT_TRUE(module != nullptr) << "LLVM Module is null";
}

TEST(LibraryIRGen, Not_RegisterOperand) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov eax, 7
  not eax
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'not'";

  std::string ir = dumpIR(mod);
  EXPECT_TRUE(ir.find("xor") != std::string::npos)
      << "Expected 'xor' (CreateNot) in IR:\n"
      << ir;
}

TEST(LibraryIRGen, Imul_TwoOperand) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov eax, 6
  mov ebx, 7
  imul eax, ebx
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'imul' (2-op)";

  std::string ir = dumpIR(mod);
  EXPECT_TRUE(ir.find("mul") != std::string::npos) << "Expected 'mul' in IR:\n"
                                                   << ir;
}

TEST(LibraryIRGen, Imul_ThreeOperand) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov ecx, 5
  imul eax, ecx, 4
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'imul' (3-op)";

  std::string ir = dumpIR(mod);
  EXPECT_TRUE(ir.find("mul") != std::string::npos) << "Expected 'mul' in IR:\n"
                                                   << ir;
}

TEST(LibraryIRGen, Cdq_SignExtends_EAX_Into_EDX) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov eax, -10
  cdq
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'cdq'";

  std::string ir = dumpIR(mod);
  EXPECT_TRUE(ir.find("ashr") != std::string::npos)
      << "Expected 'ashr' (sign replication) in IR:\n"
      << ir;
}

TEST(LibraryIRGen, Idiv_SignedDivision) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov eax, 10
  mov ebx, 3
  cdq
  idiv ebx
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'idiv'";

  std::string ir = dumpIR(mod);
  EXPECT_TRUE(ir.find("sdiv") != std::string::npos)
      << "Expected 'sdiv' in IR:\n"
      << ir;
  EXPECT_TRUE(ir.find("srem") != std::string::npos)
      << "Expected 'srem' in IR:\n"
      << ir;
}

TEST(LibraryIRGen, Idiv_NegativeDividend) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov eax, -7
  mov ecx, 2
  cdq
  idiv ecx
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr)
      << "Module generation failed for 'idiv' (negative)";

  std::string ir = dumpIR(mod);
  EXPECT_TRUE(ir.find("sdiv") != std::string::npos)
      << "Expected 'sdiv' in IR:\n"
      << ir;
}

TEST(LibraryIRGen, Movsx_SignExtend) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov ebx, -1
  movsx eax, ebx
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'movsx'";

  std::string ir = dumpIR(mod);
  EXPECT_TRUE(ir.find("eax") != std::string::npos)
      << "Expected 'eax' store in IR:\n"
      << ir;
}

TEST(LibraryIRGen, Movzx_ZeroExtend) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov ebx, 255
  movzx eax, ebx
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'movzx'";

  std::string ir = dumpIR(mod);
  EXPECT_TRUE(ir.find("eax") != std::string::npos)
      << "Expected 'eax' store in IR:\n"
      << ir;
}

TEST(LibraryIRGen, Xchg_SwapRegisters) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov eax, 10
  mov ebx, 20
  xchg eax, ebx
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'xchg'";

  std::string ir = dumpIR(mod);
  size_t storeCount = 0;
  size_t pos = 0;
  while ((pos = ir.find("store", pos)) != std::string::npos) {
    ++storeCount;
    ++pos;
  }
  EXPECT_GE(storeCount, 2u) << "Expected at least 2 stores for xchg in IR:\n"
                            << ir;
}

TEST(LibraryIRGen, Xchg_RegisterAndMemory) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov eax, 42
  xchg eax, [ebx]
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr)
      << "Module generation failed for 'xchg' (reg/mem)";
  EXPECT_FALSE(dumpIR(mod).empty());
}

TEST(LibraryIRGen, Not_MemoryOperand) {
  const std::string src = R"(
SECTION .text
global _start
_start:
  mov eax, 0
  not [eax]
  ret
)";
  llvm::Module* mod = buildModuleFromSource(src);
  ASSERT_TRUE(mod != nullptr) << "Module generation failed for 'not' (memory)";
  EXPECT_FALSE(dumpIR(mod).empty());
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
