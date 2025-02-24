#include <iostream>
#include "lexer/Lexer.h"

using namespace std;

std::string example_test_str =
    ".file \"hello.c\"\n"
    ".intel_syntax noprefix\n"
    ".text\n"
    ".section .rodata\n"
    ".LC0:\n"
    ".string \"Hello, World!\"\n"
    ".text\n"
    ".globl main\n"
    ".type main, @function\n"
    "main:\n"
    ".LFB0:\n"
    ".cfi_startproc\n"
    "endbr64\n"
    "push rbp\n"
    ".cfi_def_cfa_offset 16\n"
    ".cfi_offset 6, -16\n"
    "mov rbp, rsp\n"
    ".cfi_def_cfa_register 6\n"
    "lea rdi, .LC0[rip]\n"
    "call puts@PLT\n"
    "mov eax, 0\n"
    "pop rbp\n"
    ".cfi_def_cfa 7, 8\n"
    "ret\n"
    ".cfi_endproc\n"
    ".LFE0:\n"
    ".size main, .-main\n"
    ".ident \"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.2) 9.4.0\"\n"
    ".section .note.GNU-stack,\"\",@progbits\n"
    ".section .note.gnu.property,\"a\"\n"
    ".align 8\n"
    ".long 1f - 0f\n"
    ".long 4f - 1f\n"
    ".long 5\n"
    "0:\n"
    ".string \"GNU\"\n"
    "1:\n"
    ".align 8\n"
    ".long 0xc0000002\n"
    ".long 3f - 2f\n"
    "2:\n"
    ".long 0x3\n"
    "3:\n"
    ".align 8\n"
    "4:\n";

int main() {
  std::string input = "mov eax, 5\n ADd ebx, 10\n end";
  Lexer lexer(input);
  std::vector<Token> tokens = lexer.tokenize();

  for (const Token& token : tokens) {
    std::cout << "Token: " << token.value << " Type: " << token.type_name
              << "\n";
  }
  return 0;
}
