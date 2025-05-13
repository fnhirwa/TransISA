#include <algorithm>
#include <cctype>
#include <iostream>
#include <regex>

#include "lexer/Lexer.h"
#include "lexer/Token.h"

std::vector<std::string> punctuations =
    {"(", ")", "+", "-", "*", "/", "=", "%", "$"};
// The mapping of basic instructions and registers to their corresponding token
// types This will also store the directives and other keywords like .file,
// .section, etc.
Lexer::Lexer(const std::string_view& source) : source(source) {
  keywordTrie = std::make_unique<Trie>();
  keywordTrie->insert("mov", TokenType::INSTRUCTION);
  keywordTrie->insert("add", TokenType::INSTRUCTION);
  keywordTrie->insert("sub", TokenType::INSTRUCTION);
  keywordTrie->insert("mul", TokenType::INSTRUCTION);
  keywordTrie->insert("div", TokenType::INSTRUCTION);
  keywordTrie->insert("mod", TokenType::INSTRUCTION);
  keywordTrie->insert("end", TokenType::INSTRUCTION);
  keywordTrie->insert("lea", TokenType::INSTRUCTION);
  keywordTrie->insert("syscall", TokenType::INSTRUCTION);
  keywordTrie->insert("int", TokenType::INSTRUCTION);
  keywordTrie->insert("cmp", TokenType::INSTRUCTION);
  keywordTrie->insert("test", TokenType::INSTRUCTION);
  // FPU
  keywordTrie->insert("fadd", TokenType::INSTRUCTION); // Floating add
  keywordTrie->insert("fsub", TokenType::INSTRUCTION); // Floating subtract
  keywordTrie->insert("fmul", TokenType::INSTRUCTION); // Floating multiply
  keywordTrie->insert("fdiv", TokenType::INSTRUCTION); // Floating divide
  keywordTrie->insert("fld", TokenType::INSTRUCTION); // Load floating value
  keywordTrie->insert("fst", TokenType::INSTRUCTION); // Store floating value
  keywordTrie->insert("fstp", TokenType::INSTRUCTION); // Store and pop
  keywordTrie->insert("fcom", TokenType::INSTRUCTION); // Floating compare
  // some useful  instructions
  keywordTrie->insert("nop", TokenType::INSTRUCTION); // No operation
  keywordTrie->insert("hlt", TokenType::INSTRUCTION); // Halt CPU
  keywordTrie->insert("cli", TokenType::INSTRUCTION); // Clear interrupts
  keywordTrie->insert("sti", TokenType::INSTRUCTION); // Enable interrupts
  keywordTrie->insert("cmc", TokenType::INSTRUCTION); // Complement carry flag
  keywordTrie->insert(
      "cld", TokenType::INSTRUCTION); // Clear direction flag (for string ops)
  keywordTrie->insert("std", TokenType::INSTRUCTION); // Set direction flag
  keywordTrie->insert("enter", TokenType::INSTRUCTION); // Function prologue
  keywordTrie->insert("leave", TokenType::INSTRUCTION); // Function epilogue
  // rep-based instructions
  keywordTrie->insert(
      "movsb", TokenType::INSTRUCTION); // Move byte from [esi] to [edi]
  keywordTrie->insert("movsw", TokenType::INSTRUCTION); // Move word
  keywordTrie->insert("movsd", TokenType::INSTRUCTION); // Move dword
  keywordTrie->insert("cmpsb", TokenType::INSTRUCTION); // Compare byte
  keywordTrie->insert(
      "lodsb", TokenType::INSTRUCTION); // Load byte from [esi] into al
  keywordTrie->insert("stosb", TokenType::INSTRUCTION); // Store al into [edi]
  keywordTrie->insert(
      "scasb", TokenType::INSTRUCTION); // Scan string (compare al with [edi])
  keywordTrie->insert(
      "rep", TokenType::INSTRUCTION); // Repeat instruction (e.g., `rep movsb`)
  keywordTrie->insert("repe", TokenType::INSTRUCTION); // Repeat while equal
  keywordTrie->insert(
      "repne", TokenType::INSTRUCTION); // Repeat while not equal
  // Function call and return
  keywordTrie->insert("call", TokenType::INSTRUCTION); // Call a function
  keywordTrie->insert("ret", TokenType::INSTRUCTION); // Return from a function
  keywordTrie->insert("iret", TokenType::INSTRUCTION); // Interrupt return
  keywordTrie->insert("retn", TokenType::INSTRUCTION); // Near return (explicit)
  keywordTrie->insert(
      "retf", TokenType::INSTRUCTION); // Far return (segment change)
  // Stack operations
  keywordTrie->insert("push", TokenType::INSTRUCTION); // Push onto stack
  keywordTrie->insert("pop", TokenType::INSTRUCTION); // Pop from stack
  keywordTrie->insert(
      "pusha",
      TokenType::INSTRUCTION); // Push all general-purpose regs (16/32-bit)
  keywordTrie->insert(
      "popa",
      TokenType::INSTRUCTION); // Pop all general-purpose regs (16/32-bit)
  keywordTrie->insert("pushad", TokenType::INSTRUCTION); // Push all (32-bit)
  keywordTrie->insert("popad", TokenType::INSTRUCTION); // Pop all (32-bit)
  keywordTrie->insert("pushf", TokenType::INSTRUCTION); // Push flags register
  keywordTrie->insert("popf", TokenType::INSTRUCTION); // Pop flags register
  keywordTrie->insert("dec", TokenType::INSTRUCTION); // Decrement
  keywordTrie->insert("inc", TokenType::INSTRUCTION); // Increment

  // Jump instructions
  keywordTrie->insert("jmp", TokenType::INSTRUCTION); // Unconditional jump
  keywordTrie->insert("je", TokenType::INSTRUCTION); // Jump if equal
  keywordTrie->insert("jne", TokenType::INSTRUCTION); // Jump if not equal
  keywordTrie->insert("jg", TokenType::INSTRUCTION); // Jump if greater
  keywordTrie->insert(
      "jge", TokenType::INSTRUCTION); // Jump if greater or equal
  keywordTrie->insert("jl", TokenType::INSTRUCTION); // Jump if less
  keywordTrie->insert("jle", TokenType::INSTRUCTION); // Jump if less or equal
  // Equality/Zero-based jumps
  keywordTrie->insert(
      "jz", TokenType::INSTRUCTION); // Jump if zero (same as je)
  keywordTrie->insert(
      "jnz", TokenType::INSTRUCTION); // Jump if not zero (same as jne)

  // Unsigned comparisons (for "above/below")
  keywordTrie->insert(
      "ja", TokenType::INSTRUCTION); // Jump if above (unsigned >)
  keywordTrie->insert(
      "jae", TokenType::INSTRUCTION); // Jump if above or equal (unsigned >=)
  keywordTrie->insert(
      "jb", TokenType::INSTRUCTION); // Jump if below (unsigned <)
  keywordTrie->insert(
      "jbe", TokenType::INSTRUCTION); // Jump if below or equal (unsigned <=)

  keywordTrie->insert(
      "loop", TokenType::INSTRUCTION); // Decrement ecx/rcx and jump if not zero
  keywordTrie->insert(
      "loope", TokenType::INSTRUCTION); // Loop while equal (ZF=1)
  keywordTrie->insert(
      "loopne", TokenType::INSTRUCTION); // Loop while not equal (ZF=0)

  // Bitwise operations
  keywordTrie->insert("xor", TokenType::INSTRUCTION); // Bitwise XOR
  keywordTrie->insert("and", TokenType::INSTRUCTION); // Bitwise AND
  keywordTrie->insert("or", TokenType::INSTRUCTION); // Bitwise OR
  keywordTrie->insert("not", TokenType::INSTRUCTION); // Bitwise NOT
  keywordTrie->insert("shl", TokenType::INSTRUCTION); // Shift left
  keywordTrie->insert("shr", TokenType::INSTRUCTION); // Shift right (logical)
  keywordTrie->insert(
      "sar", TokenType::INSTRUCTION); // Shift right (arithmetic)
  keywordTrie->insert("rol", TokenType::INSTRUCTION); // Rotate left
  keywordTrie->insert("ror", TokenType::INSTRUCTION); // Rotate right

  // Signed comparisons (extended)
  keywordTrie->insert("jo", TokenType::INSTRUCTION); // Jump if overflow
  keywordTrie->insert("jno", TokenType::INSTRUCTION); // Jump if no overflow
  keywordTrie->insert("js", TokenType::INSTRUCTION); // Jump if sign (negative)
  keywordTrie->insert(
      "jns", TokenType::INSTRUCTION); // Jump if not sign (positive)
  // 8-bit registers (lower parts)
  keywordTrie->insert("al", TokenType::REGISTER);
  keywordTrie->insert("bl", TokenType::REGISTER);
  keywordTrie->insert("cl", TokenType::REGISTER);
  keywordTrie->insert("dl", TokenType::REGISTER);

  // 8-bit registers (high parts, 16/32-bit only)
  keywordTrie->insert("ah", TokenType::REGISTER);
  keywordTrie->insert("bh", TokenType::REGISTER);
  keywordTrie->insert("ch", TokenType::REGISTER);
  keywordTrie->insert("dh", TokenType::REGISTER);

  // 16-bit registers
  keywordTrie->insert("ax", TokenType::REGISTER);
  keywordTrie->insert("bx", TokenType::REGISTER);
  keywordTrie->insert("cx", TokenType::REGISTER);
  keywordTrie->insert("dx", TokenType::REGISTER);
  keywordTrie->insert("si", TokenType::REGISTER); // Lower 16 of esi
  keywordTrie->insert("di", TokenType::REGISTER); // Lower 16 of edi
  keywordTrie->insert("bp", TokenType::REGISTER); // Lower 16 of ebp
  keywordTrie->insert("sp", TokenType::REGISTER); // Lower 16 of esp

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
  keywordTrie->insert("r8b", TokenType::REGISTER);
  // 64-bit additional 8-bit registers
  keywordTrie->insert("r9b", TokenType::REGISTER);
  keywordTrie->insert("r10b", TokenType::REGISTER);
  keywordTrie->insert("r11b", TokenType::REGISTER);
  keywordTrie->insert("r12b", TokenType::REGISTER);
  keywordTrie->insert("r13b", TokenType::REGISTER);
  keywordTrie->insert("r14b", TokenType::REGISTER);
  keywordTrie->insert("r15b", TokenType::REGISTER);
  // 64-bit additional 16-bit registers
  keywordTrie->insert("r8w", TokenType::REGISTER);
  keywordTrie->insert("r9w", TokenType::REGISTER);
  keywordTrie->insert("r10w", TokenType::REGISTER);
  keywordTrie->insert("r11w", TokenType::REGISTER);
  keywordTrie->insert("r12w", TokenType::REGISTER);
  keywordTrie->insert("r13w", TokenType::REGISTER);
  keywordTrie->insert("r14w", TokenType::REGISTER);
  keywordTrie->insert("r15w", TokenType::REGISTER);
  // 64-bit additional 32-bit registers
  keywordTrie->insert("r8d", TokenType::REGISTER);
  keywordTrie->insert("r9d", TokenType::REGISTER);
  keywordTrie->insert("r10d", TokenType::REGISTER);
  keywordTrie->insert("r11d", TokenType::REGISTER);
  keywordTrie->insert("r12d", TokenType::REGISTER);
  keywordTrie->insert("r13d", TokenType::REGISTER);
  keywordTrie->insert("r14d", TokenType::REGISTER);
  keywordTrie->insert("r15d", TokenType::REGISTER);

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

  // Assembler Directives
  keywordTrie->insert(".file", TokenType::DIRECTIVE);
  keywordTrie->insert(".intel_syntax", TokenType::DIRECTIVE);
  keywordTrie->insert(".type", TokenType::DIRECTIVE);
  keywordTrie->insert(".globl", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_startproc", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_def_cfa_offset", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_endproc", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_offset", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_def_cfa_register", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_def_cfa", TokenType::DIRECTIVE);
  keywordTrie->insert(".size", TokenType::DIRECTIVE);
  keywordTrie->insert(".ident", TokenType::DIRECTIVE);
  keywordTrie->insert(".align", TokenType::DIRECTIVE);
  keywordTrie->insert(".long", TokenType::DIRECTIVE);
  keywordTrie->insert(".byte", TokenType::DIRECTIVE);
  keywordTrie->insert(".short", TokenType::DIRECTIVE);
  keywordTrie->insert(".quad", TokenType::DIRECTIVE);
  keywordTrie->insert(".zero", TokenType::DIRECTIVE);
  keywordTrie->insert(".asciz", TokenType::DIRECTIVE);
  keywordTrie->insert(".note", TokenType::DIRECTIVE);
  keywordTrie->insert(".GNU-stack", TokenType::DIRECTIVE);
  keywordTrie->insert(".note.GNU-stack", TokenType::DIRECTIVE);
  keywordTrie->insert(".note.gnu.property", TokenType::DIRECTIVE);
  keywordTrie->insert(".string", TokenType::DIRECTIVE);
  keywordTrie->insert("global", TokenType::DIRECTIVE);
  keywordTrie->insert("resb", TokenType::DIRECTIVE);
  keywordTrie->insert("resw", TokenType::DIRECTIVE);
  keywordTrie->insert("resd", TokenType::DIRECTIVE);
  keywordTrie->insert("resq", TokenType::DIRECTIVE);
  keywordTrie->insert("rest", TokenType::DIRECTIVE);

  // Segment Directives
  keywordTrie->insert(".text", TokenType::SEGMENT_DIRECTIVE);
  keywordTrie->insert(".data", TokenType::SEGMENT_DIRECTIVE);
  keywordTrie->insert(".bss", TokenType::SEGMENT_DIRECTIVE);
  keywordTrie->insert(".section", TokenType::SEGMENT_DIRECTIVE);
  keywordTrie->insert(".rodata", TokenType::SEGMENT_DIRECTIVE);
  keywordTrie->insert("section", TokenType::SEGMENT_DIRECTIVE);
  keywordTrie->insert(".string", TokenType::SEGMENT_DIRECTIVE);
  // Data Value
  keywordTrie->insert("db", TokenType::DATA_VALUE);
  keywordTrie->insert("dw", TokenType::DATA_VALUE);
  keywordTrie->insert("dd", TokenType::DATA_VALUE);
  // Special Punctuations
  keywordTrie->insert(",", TokenType::COMMA);
  keywordTrie->insert(":", TokenType::COLON);
  keywordTrie->insert("[", TokenType::L_BRACKET);
  keywordTrie->insert("]", TokenType::R_BRACKET);
}

// Report an error within the lexer
void Lexer::reportError(const std::string& message) {
  std::cerr << "Lexer Error [Line " << line << ", Column " << column
            << "]: " << message << std::endl;
  exit(EXIT_FAILURE); // Stop execution on lexing error
}

std::string tokenValueToLower(std::string_view str) {
  std::string result(str);
  std::transform(result.begin(), result.end(), result.begin(), ::tolower);
  return result;
}

// Return the next character in the input string
char Lexer::peek() {
  return position < source.size() ? source[position] : '\0';
}

// Advance the position in the input string and return the character
char Lexer::advance() {
  if (position < source.size()) {
    char c = source[position++];
    column++;
    return c;
  }
  return '\0';
}

// Skip whitespace characters in the input string
void Lexer::skipWhitespace() {
  while (position < source.size() && isspace(peek())) {
    if (peek() == '\n') {
      line++;
      column = 0;
    }
    advance();
  }
}

// Skip comments in the input string by moving to the next line
void Lexer::skipComment() {
  if (peek() == ';') {
    while (peek() != '\n' && peek() != '\0') {
      advance();
    }
    skipWhitespace();
  }
}

//=============================================================//
//                 Reading different tokens                   //
//===========================================================//

// Read an instruction from the input string
Token Lexer::readInstruction() {
  std::string value;
  while (position < source.size() && isalnum(source[position])) {
    value += advance();
  }

  // Check if it's a known instruction
  std::string lower = tokenValueToLower(value);
  TokenType type = keywordTrie->find(lower);

  if (type == TokenType::INSTRUCTION) {
    return {type, lower, "INSTRUCTION", line, column};
  } else {
    reportError("Unknown instruction: " + lower);
  }
  return {TokenType::INVALID, lower, "INVALID", line, column};
}

// Read a register from the input string
Token Lexer::readRegister() {
  std::string value;
  while (position < source.size() && isalnum(source[position])) {
    value += advance();
  }
  std::string lower = tokenValueToLower(value);
  if (keywordTrie->find(lower) == TokenType::REGISTER) {
    return {TokenType::REGISTER, lower, "REGISTER", line, column};
  } else {
    reportError("Unknown register: " + lower);
  }
  return {TokenType::INVALID, lower, "INVALID", line, column};
}

// Detect the type of the number (decimal, hex, octal, binary)
std::string detectNumberType(const std::string& value) {
  static const std::regex decimalRegex(R"(^[+-]?[0-9]+$)");
  static const std::regex hexRegex(R"(^0x[0-9A-Fa-f]+$|^[0-9A-Fa-f]+h$)");
  static const std::regex octalRegex(R"(^0[0-7]+$)");
  static const std::regex binaryRegex(R"(^0b[01]+$)");

  if (std::regex_match(value, decimalRegex)) {
    return "Decimal";
  } else if (std::regex_match(value, hexRegex)) {
    return "Hexadecimal";
  } else if (std::regex_match(value, octalRegex)) {
    return "Octal";
  } else if (std::regex_match(value, binaryRegex)) {
    return "Binary";
  } else {
    return "Unknown format";
  }
}

// Read an immediate value from the input string
Token Lexer::readImmediate() {
  std::string value; // Store the immediate as a string

  // Handle optional '+' or '-' sign
  if (peek() == '+' || peek() == '-') {
    value += advance();
    while (isdigit(peek())) {
      value += advance();
    }
    return Token{TokenType::IMMEDIATE, value, "IMMEDIATE", line, column};
  }
  // hex has A, B, C, D, E, F
  std::vector<char> hex_chars = {
      'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F'};
  // Hexadecimal number (starts with 0x or 0X) or has 'h' at the end
  if (peek() == '0' &&
      (source[position + 1] == 'x' || source[position + 1] == 'X')) {
    value += advance(); // Consume '0'
    value += advance(); // Consume 'x'
    while (isdigit(peek())) {
      value += advance();
    }
    while (std::find(hex_chars.begin(), hex_chars.end(), peek()) !=
           hex_chars.end()) {
      value += advance();
    }
    return Token{
        TokenType::HEX_IMMEDIATE, value, "HEX_IMMEDIATE", line, column};
  }
  // binary values are represented as 0b1010
  if (peek() == '0' &&
      (source[position + 1] == 'b' || source[position + 1] == 'B')) {
    value += advance(); // Consume '0'
    value += advance(); // Consume 'b'
    while (source[position] == '0' || source[position] == '1') {
      value += advance();
    }
    return Token{
        TokenType::BIN_IMMEDIATE, value, "BIN_IMMEDIATE", line, column};
  }
  // octal values are represented as 0o1234
  if (peek() == '0' &&
      (source[position + 1] == 'o' || source[position + 1] == 'O' ||
       source[position + 1] == 'q')) {
    value += advance(); // Consume '0'
    value += advance(); // Consume 'o', 'O' or 'q'
    while (source[position] >= '0' && source[position] <= '7') {
      value += advance();
    }
    return Token{
        TokenType::OCT_IMMEDIATE, value, "OCT_IMMEDIATE", line, column};
  }

  // Decimal number (starts with 1-9 or a sign)
  if (isdigit(peek())) {
    while (isdigit(peek())) {
      value += advance();
    }
    // check if the next character is a hex character
    while (std::find(hex_chars.begin(), hex_chars.end(), peek()) !=
           hex_chars.end()) {
      value += advance();
    }
    if (peek() == 'h') {
      value += advance();
      return Token{
          TokenType::HEX_IMMEDIATE, value, "HEX_IMMEDIATE", line, column};
    } else if (peek() == 'o' || peek() == 'q') {
      value += advance();
      return Token{
          TokenType::OCT_IMMEDIATE, value, "OCT_IMMEDIATE", line, column};
    }
    return Token{TokenType::IMMEDIATE, value, "IMMEDIATE", line, column};
  }
  return Token{TokenType::INVALID, value, "INVALID", line, column};
}

// Read a label from the input string
Token Lexer::readLabel() {
  std::string value;
  if (peek() == '.' || peek() == '_') {
    value += advance();
  }
  while (position < source.size() &&
         (isalnum(source[position]) || source[position] == '_')) {
    value += advance();
  }
  if (peek() == ':') {
    advance(); // Consume ':'
    return {TokenType::LABEL, value, "LABEL", line, column};
  } else if (peek() == ' ') {
    skipWhitespace();
  }
  return {TokenType::INVALID, value, "INVALID", line, column};
}

// Identifier
Token Lexer::readIdentifier() {
  std::string value;
  while (isalnum(peek()) || peek() == '_') {
    value += advance();
  }
  return {TokenType::IDENTIFIER, value, "IDENTIFIER", line, column};
}

// Reading Assembler Directives
Token Lexer::readDirective() {
  std::string value;
  // the directive is started with a '.'
  // so we need to add it to the value
  value += advance();
  while (isalnum(peek()) || peek() == '_' || peek() == '-' || peek() == '.') {
    value += advance();
  }
  std::string lower = tokenValueToLower(value);
  TokenType type = keywordTrie->find(lower);
  if (type == TokenType::DIRECTIVE) {
    return {type, lower, "DIRECTIVE", line, column};
  }
  if (type == TokenType::SEGMENT_DIRECTIVE) {
    return {type, lower, "SEGMENT_DIRECTIVE", line, column};
  }
  // handle values like .-main, _start
  return {TokenType::IDENTIFIER, lower, "IDENTIFIER", line, column};
}

// Read any other keyword from the input string
Token Lexer::readValue() {
  std::string keyword;
  // Read the keyword until a non-alphanumeric character is found
  while (isalnum(peek()) || peek() == '_') {
    keyword += advance();
  }
  std::string keyword_lower = tokenValueToLower(keyword);
  TokenType type = keywordTrie->find(keyword_lower);
  if (type != TokenType::INVALID) {
    std::string name = getTokenTypeName(type);
    return {type, keyword_lower, name, line, column};
  }
  std::string numberType = detectNumberType(keyword_lower);
  if (numberType == "Decimal") {
    return Token{
        TokenType::IMMEDIATE, keyword_lower, "IMMEDIATE", line, column};
  }
  if (numberType == "Hexadecimal") {
    return Token{
        TokenType::HEX_IMMEDIATE, keyword_lower, "HEX_IMMEDIATE", line, column};
  }
  if (numberType == "Binary") {
    return Token{
        TokenType::BIN_IMMEDIATE, keyword_lower, "BIN_IMMEDIATE", line, column};
  }
  if (numberType == "Octal") {
    return Token{
        TokenType::OCT_IMMEDIATE, keyword_lower, "OCT_IMMEDIATE", line, column};
  }
  if (keywordTrie->find(keyword_lower) == TokenType::DIRECTIVE) {
    return Token{
        TokenType::DIRECTIVE, keyword_lower, "DIRECTIVE", line, column};
  }
  if (keywordTrie->find(keyword_lower) == TokenType::SEGMENT_DIRECTIVE) {
    return Token{
        TokenType::SEGMENT_DIRECTIVE,
        keyword_lower,
        "SEGMENT_DIRECTIVE",
        line,
        column};
  }
  if (keywordTrie->find(keyword_lower) == TokenType::DATA_VALUE) {
    return Token{
        TokenType::DATA_VALUE, keyword_lower, "DATA_VALUE", line, column};
  }
  return Token{TokenType::IDENTIFIER, keyword, "IDENTIFIER", line, column};
}

// Comma
Token Lexer::readComma() {
  std::string value;
  value += advance();
  return Token{TokenType::COMMA, value, "COMMA", line, column};
}

// Colon
Token Lexer::readColon() {
  std::string value;
  value += advance();
  return Token{TokenType::COLON, value, "COLON", line, column};
}

// Read the Left Bracket '['
Token Lexer::readLeftBracket() {
  std::string value;
  value += advance();
  return Token{TokenType::L_BRACKET, value, "L_BRACKET", line, column};
}

// Read the right Bracket ']'
Token Lexer::readRightBracket() {
  std::string value;
  value += advance();
  return Token{TokenType::R_BRACKET, value, "R_BRACKET", line, column};
}

// Read a punctuation character from the input string
Token Lexer::readPunctuation() {
  char c = advance();
  return {
      TokenType::PUNCTUATION, std::string(1, c), "PUNCTUATION", line, column};
}

// Read a string for some cases we have strings in the code
Token Lexer::readString() {
  std::string value;
  // the string is started with a '"'
  // so we need to add it to the value
  char quote = advance();
  value += quote;
  while (peek() != quote && peek() != '\0') {
    value += advance();
  }
  // the string is ended with a '"'
  // so we need to add it to the value
  value += advance();
  // remove the first and last character and add new line as a terminator
  value = value.substr(1, value.size() - 2) + '\n';
  return {TokenType::STRING, value, "STRING", line, column};
}

//==========================================================//
//                    Tokenization                          //
//=========================================================//

// Tokenize the input string
std::vector<Token> Lexer::tokenize() {
  std::vector<Token> tokens;

  while (position < source.size()) {
    skipWhitespace(); // Then skip remaining spaces
    while (peek() == ';') { // Ensure all comments are skipped
      skipComment();
      skipWhitespace(); // Also clear trailing spaces after a comment
    }
    if (position >= source.size())
      break;

    char c = peek();

    if (isalnum(c) || c == '.' || c == '_') {
      size_t nextWhitespace = source.find_first_of(" \t\n", position);
      size_t nextColon = source.find(':', position);

      if (c == '.' || "_") {
        size_t nextBracket = source.find('[', position);
        if (nextColon != std::string::npos && nextColon < nextWhitespace) {
          tokens.push_back(readLabel());
        } else if (
            nextBracket != std::string::npos && nextBracket < nextWhitespace) {
          tokens.push_back(readLabel());
        } else {
          if (c == '.' || c == '_') {
            tokens.push_back(readDirective());
          } else {
            tokens.push_back(readValue());
          }
        }
      } else if (nextColon != std::string::npos && nextColon < nextWhitespace) {
        tokens.push_back(readLabel());
      } else {
        tokens.push_back(readValue());
      }
    } else if (c == '"' || c == '\'') {
      tokens.push_back(readString());
    } else if (
        isdigit(c) ||
        (c == '0' && position + 1 < source.size() &&
         (source[position + 1] == 'x' || source[position + 1] == 'X' ||
          source[position + 1] == 'b' || source[position + 1] == 'o' ||
          source[position + 1] == 'O' || source[position + 1] == 'q')) ||
        (c == '-' && position + 1 < source.size() &&
         isdigit(source[position + 1]))) {
      tokens.push_back(readImmediate());
    } else if (c == ',') {
      tokens.push_back(readComma());
    } else if (c == ':') {
      tokens.push_back(readColon());
    } else if (c == '[') {
      tokens.push_back(readLeftBracket());
    } else if (c == ']') {
      tokens.push_back(readRightBracket());
    } else if (c == ';') {
      skipComment();
    } else if (
        std::find(
            punctuations.begin(), punctuations.end(), std::string(1, c)) !=
        punctuations.end()) {
      tokens.push_back(readPunctuation());
    } else {
      reportError("Unexpected character: " + std::string(1, c));
    }
  }

  tokens.push_back({TokenType::END, "END", "END", line, column});
  return tokens;
}

// Read the source file and return the content as a string
std::string readFileToString(const std::string& filePath) {
  if (!isValidFileExtension(filePath)) {
    throw std::runtime_error("Invalid file extension: " + filePath);
  }
  std::ifstream file(filePath, std::ios::binary);
  if (!file.is_open()) {
    throw std::runtime_error("Failed to open file: " + filePath);
  }
  std::stringstream buffer;
  buffer << file.rdbuf();
  return buffer.str();
}

// Read the source string and return it as a string_view
std::string_view readFileToStringView(
    const std::string& filePath,
    std::string& bufferStorage) {
  if (!isValidFileExtension(filePath)) {
    throw std::runtime_error("Invalid file extension: " + filePath);
  }
  std::ifstream file(filePath, std::ios::binary);
  if (!file.is_open()) {
    throw std::runtime_error("Failed to open file: " + filePath);
  }
  std::stringstream buffer;
  buffer << file.rdbuf();
  bufferStorage = buffer.str();
  return bufferStorage;
}

// Check if the file extension is valid
bool isValidFileExtension(const std::string& filePath) {
  size_t dotPos = filePath.find_last_of(".");
  if (dotPos == std::string::npos) {
    return ""; // No extension found
  }
  std::string extension = filePath.substr(dotPos + 1);
  std::transform(
      extension.begin(), extension.end(), extension.begin(), ::tolower);
  return extension == "s" || extension == "asm" || extension == "S";
}