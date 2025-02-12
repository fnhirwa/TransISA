#include "lexer/Lexer.h"
#include <cctype>
#include <iostream>

std::vector<std::string> punctuations =
    {",", ":", "(", ")", "[", "]", "+", "-", "*", "/", "=", "%", "$"};

Lexer::Lexer(const std::string& source) : source(source) {
  keywordTrie = std::make_unique<Trie>();
  keywordTrie->insert("mov", TokenType::INSTRUCTION);
  keywordTrie->insert("add", TokenType::INSTRUCTION);
  keywordTrie->insert("sub", TokenType::INSTRUCTION);
  keywordTrie->insert("mul", TokenType::INSTRUCTION);
  keywordTrie->insert("div", TokenType::INSTRUCTION);
  keywordTrie->insert("mod", TokenType::INSTRUCTION);
  keywordTrie->insert("push", TokenType::INSTRUCTION);
  keywordTrie->insert("pop", TokenType::INSTRUCTION);
  keywordTrie->insert("jmp", TokenType::INSTRUCTION);
  keywordTrie->insert("je", TokenType::INSTRUCTION);
  keywordTrie->insert("jne", TokenType::INSTRUCTION);
  keywordTrie->insert("jg", TokenType::INSTRUCTION);
  keywordTrie->insert("jge", TokenType::INSTRUCTION);
  keywordTrie->insert("jl", TokenType::INSTRUCTION);
  keywordTrie->insert("jle", TokenType::INSTRUCTION);
  keywordTrie->insert("call", TokenType::INSTRUCTION);
  keywordTrie->insert("ret", TokenType::INSTRUCTION);
  keywordTrie->insert("end", TokenType::INSTRUCTION);
  // General-purpose registers (32-bit)
  keywordTrie->insert("eax", TokenType::REGISTER);
  keywordTrie->insert("ebx", TokenType::REGISTER);
  keywordTrie->insert("ecx", TokenType::REGISTER);
  keywordTrie->insert("edx", TokenType::REGISTER);
  keywordTrie->insert("esi", TokenType::REGISTER);
  keywordTrie->insert("edi", TokenType::REGISTER);
  keywordTrie->insert("ebp", TokenType::REGISTER);
  keywordTrie->insert("esp", TokenType::REGISTER);

  // General-purpose registers (64-bit)
  keywordTrie->insert("rax", TokenType::REGISTER);
  keywordTrie->insert("rbx", TokenType::REGISTER);
  keywordTrie->insert("rcx", TokenType::REGISTER);
  keywordTrie->insert("rdx", TokenType::REGISTER);
  keywordTrie->insert("rsi", TokenType::REGISTER);
  keywordTrie->insert("rdi", TokenType::REGISTER);
  keywordTrie->insert("rbp", TokenType::REGISTER);
  keywordTrie->insert("rsp", TokenType::REGISTER);
  keywordTrie->insert("r8", TokenType::REGISTER);
  keywordTrie->insert("r9", TokenType::REGISTER);
  keywordTrie->insert("r10", TokenType::REGISTER);
  keywordTrie->insert("r11", TokenType::REGISTER);
  keywordTrie->insert("r12", TokenType::REGISTER);
  keywordTrie->insert("r13", TokenType::REGISTER);
  keywordTrie->insert("r14", TokenType::REGISTER);
  keywordTrie->insert("r15", TokenType::REGISTER);

  // Segment registers
  keywordTrie->insert("cs", TokenType::REGISTER);
  keywordTrie->insert("ds", TokenType::REGISTER);
  keywordTrie->insert("ss", TokenType::REGISTER);
  keywordTrie->insert("es", TokenType::REGISTER);
  keywordTrie->insert("fs", TokenType::REGISTER);
  keywordTrie->insert("gs", TokenType::REGISTER);

  // Control registers
  keywordTrie->insert("cr0", TokenType::REGISTER);
  keywordTrie->insert("cr1", TokenType::REGISTER);
  keywordTrie->insert("cr2", TokenType::REGISTER);
  keywordTrie->insert("cr3", TokenType::REGISTER);
  keywordTrie->insert("cr4", TokenType::REGISTER);
  keywordTrie->insert("cr8", TokenType::REGISTER);

  // Debug registers
  keywordTrie->insert("dr0", TokenType::REGISTER);
  keywordTrie->insert("dr1", TokenType::REGISTER);
  keywordTrie->insert("dr2", TokenType::REGISTER);
  keywordTrie->insert("dr3", TokenType::REGISTER);
  keywordTrie->insert("dr6", TokenType::REGISTER);
  keywordTrie->insert("dr7", TokenType::REGISTER);

  // Floating-Point Unit (FPU) registers
  keywordTrie->insert("st0", TokenType::REGISTER);
  keywordTrie->insert("st1", TokenType::REGISTER);
  keywordTrie->insert("st2", TokenType::REGISTER);
  keywordTrie->insert("st3", TokenType::REGISTER);
  keywordTrie->insert("st4", TokenType::REGISTER);
  keywordTrie->insert("st5", TokenType::REGISTER);
  keywordTrie->insert("st6", TokenType::REGISTER);
  keywordTrie->insert("st7", TokenType::REGISTER);

  // SIMD (SSE, AVX, AVX-512) registers
  for (int i = 0; i < 16; ++i) {
    keywordTrie->insert("xmm" + std::to_string(i), TokenType::REGISTER);
    keywordTrie->insert("ymm" + std::to_string(i), TokenType::REGISTER);
  }
  for (int i = 0; i < 32; ++i) {
    keywordTrie->insert("zmm" + std::to_string(i), TokenType::REGISTER);
  }
  // punctuations
  for (const std::string& punctuation : punctuations) {
    keywordTrie->insert(punctuation, TokenType::PUNCTUATION);
  }
}

void Lexer::reportError(const std::string& message) {
  std::cerr << "Lexer Error [Line " << line << ", Column " << column
            << "]: " << message << std::endl;
  exit(EXIT_FAILURE); // Stop execution on lexing error
}

char Lexer::peek() {
  return position < source.size() ? source[position] : '\0';
}

char Lexer::advance() {
  if (position < source.size()) {
    char c = source[position++];
    column++;
    return c;
  }
  return '\0';
}

void Lexer::skipWhitespace() {
  while (isspace(peek())) {
    if (peek() == '\n') {
      line++;
      column = 1;
    }
    advance();
  }
}

void Lexer::skipComment() {
  if (peek() == ';') {
    while (peek() != '\n' && peek() != '\0') {
      advance();
    }
  }
}

Token Lexer::readInstruction() {
  std::string value;

  while (position < source.size() && isalnum(source[position])) {
    value += advance();
  }

  // Check if it's a known instruction
  TokenType type = keywordTrie->find(value);

  if (type == TokenType::INSTRUCTION) {
    return {type, value, line, column};
  } else {
    reportError("Unknown instruction: " + value);
  }
  return {TokenType::END_OF_FILE, "", line, column};
}

Token Lexer::readKeyword() {
  std::string keyword;
  while (isalnum(peek())) {
    keyword += advance();
  }
  TokenType type = keywordTrie->find(keyword);
  return Token{type, keyword, line, column};
}

Token Lexer::readRegister() {
  std::string value;

  while (position < source.size() && isalnum(source[position])) {
    value += advance();
  }

  if (keywordTrie->find(value) == TokenType::REGISTER) {
    return {TokenType::REGISTER, value, line, column};
  } else {
    reportError("Unknown register: " + value);
  }
  return {TokenType::END_OF_FILE, "", line, column};
}

Token Lexer::readImmediate() {
  size_t start = position;
  size_t startColumn = column;
  std::string value; // Store the immediate as a string

  // Handle optional '+' or '-' sign
  if (peek() == '+' || peek() == '-') {
    value += advance();
  }

  if (peek() == '0') {
    // Check for hexadecimal, binary, or octal prefix
    char next = source[position + 1];
    if (next == 'x' || next == 'X') {
      // Hexadecimal: 0x...
      value += advance(); // '0'
      value += advance(); // 'x'
      while (isxdigit(peek()))
        value += advance();
    } else if (next == 'b' || next == 'B') {
      // Binary: 0b...
      value += advance(); // '0'
      value += advance(); // 'b'
      while (peek() == '0' || peek() == '1')
        value += advance();
    } else if (isdigit(next) && next >= '0' && next <= '7') {
      // Octal: Starts with 0 and followed by 0-7
      value += advance();
      while (peek() >= '0' && peek() <= '7')
        value += advance();
    } else {
      // Single '0'
      value += advance();
    }
  } else {
    // Decimal number (starts with 1-9 or a sign)
    while (isdigit(peek()))
      value += advance();
  }

  return Token{TokenType::IMMEDIATE, value, line, startColumn};
}

Token Lexer::readPunctuation() {
  char c = advance();
  return {TokenType::PUNCTUATION, std::string(1, c), line, column};
}

Token Lexer::readLabel() {
  std::string value;

  while (position < source.size() && isalnum(source[position])) {
    value += advance();
  }

  if (peek() == ':') {
    advance(); // Consume ':'
    return {TokenType::LABEL, value, line, column};
  } else {
    reportError("Invalid label format.");
  }
  return {TokenType::END_OF_FILE, "", line, column};
}

std::vector<Token> Lexer::tokenize() {
  std::vector<Token> tokens;

  while (position < source.size()) {
    skipWhitespace();
    skipComment();

    if (position >= source.size())
      break;

    char c = peek();

    if (isalpha(c)) {
      if (source.find(':', position) != std::string::npos) {
        tokens.push_back(readLabel());
      } else {
        tokens.push_back(readKeyword());
      }
    } else if (
        isdigit(c) ||
        (c == '0' &&
         (source[position + 1] == 'x' || source[position + 1] == 'X'))) {
      tokens.push_back(readImmediate());
    } else {
      tokens.push_back(readPunctuation());
    }
  }

  tokens.push_back({TokenType::END_OF_FILE, "", line, column});
  return tokens;
}
