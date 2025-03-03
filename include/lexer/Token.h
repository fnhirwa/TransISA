// The TokenType enum class is used to represent the type of token that the
// lexer has identified. The Token struct is used to store the token's value,
// type, and position in the input string. The getTokenTypeName function is used
// to convert the TokenType enum value to a string representation.
#ifndef TOKEN_H
#define TOKEN_H

#include <string>

// This is intended to handle addressing modes
// for x86-64 and ARM architectures
// Which is initially made as Instrutions in terms of token type as for
// The AST generation they need to be computed as addressing modes
enum AddressingMode {
  IMMEDIATE, // mov eax, 10
  REGISTER, // mov rax, rbx
  DIRECT, // mov eax, [0x1000]
  INDIRECT, // mov eax, [rbx]
  BASE_DISPLACEMENT, // mov eax, [rbp - 8]
  INDEXED, // mov eax, [rbx + rcx*4]
  RIP_RELATIVE, // mov rax, [symbol[rip]]  (x86-64 only)
  PC_RELATIVE, // ldr r0, [pc, #8]  (ARM)
  STACK, // push rax / pop rbx
  AUTO_INCREMENT, // ldr r0, [r1], #4  (ARM)
  AUTO_DECREMENT // ldr r0, [r1, #-4]! (ARM pre-decrement)
};

enum class TokenType {
  INSTRUCTION,
  REGISTER,
  IMMEDIATE,
  PUNCTUATION,
  LABEL,
  INVALID,
  DIRECTIVE,
  STRING,
  VALUE,
  END,
};

struct Token {
  TokenType type;
  std::string value;
  std::string type_name;
  size_t line;
  size_t column;
};

std::string getTokenTypeName(TokenType tok);

#endif // TOKEN_H
