#include "lexer/Lexer.h"
#include <algorithm>
#include <cctype>
#include <iostream>
#include "lexer/Token.h"

std::vector<std::string> punctuations =
    {",", ":", "(", ")", "[", "]", "+", "-", "*", "/", "=", "%", "$"};
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
  keywordTrie->insert("lea", TokenType::INSTRUCTION);
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

  // Directives
  keywordTrie->insert(".file", TokenType::DIRECTIVE);
  keywordTrie->insert(".text", TokenType::DIRECTIVE);
  keywordTrie->insert(".intel_syntax", TokenType::DIRECTIVE);
  keywordTrie->insert(".section", TokenType::DIRECTIVE);
  keywordTrie->insert(".string", TokenType::DIRECTIVE);
  keywordTrie->insert(".type", TokenType::DIRECTIVE);
  keywordTrie->insert(".globl", TokenType::DIRECTIVE);
  keywordTrie->insert(".rodata", TokenType::DIRECTIVE);
  keywordTrie->insert(".type", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_startproc", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_def_cfa_offset", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_endproc", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_offset", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_def_cfa_register", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_def_cfa", TokenType::DIRECTIVE);
  keywordTrie->insert(".cfi_endproc", TokenType::DIRECTIVE);
  keywordTrie->insert(".size", TokenType::DIRECTIVE);
  keywordTrie->insert(".ident", TokenType::DIRECTIVE);
  keywordTrie->insert(".align", TokenType::DIRECTIVE);
  keywordTrie->insert(".long", TokenType::DIRECTIVE);
  keywordTrie->insert(".note", TokenType::DIRECTIVE);
  keywordTrie->insert(".GNU-stack", TokenType::DIRECTIVE);
  keywordTrie->insert(".note.GNU-stack", TokenType::DIRECTIVE);
  keywordTrie->insert(".note.gnu.property", TokenType::DIRECTIVE);
  keywordTrie->insert(".long", TokenType::DIRECTIVE);
  keywordTrie->insert(".string", TokenType::DIRECTIVE);
  // Directives
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
  while (isspace(peek())) {
    if (peek() == '\n') {
      line++;
      column = 1;
    }
    advance();
  }
}

// Skip comments in the input string
void Lexer::skipComment() {
  if (peek() == ';') {
    while (peek() != '\n' && peek() != '\0') {
      advance();
    }
  }
}

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

bool isImmediate(std::string_view value) {
  for (size_t i = 0; i < value.size(); i++) {
    if (!isdigit(value[i])) {
      return false;
    }
  }
  return true;
}

// Read any other keyword from the input string
Token Lexer::readKeyword() {
  std::string keyword;
  while (isalnum(peek()) || peek() == '_') {
    keyword += advance();
  }
  std::string keyword_lower = tokenValueToLower(keyword);
  TokenType type = keywordTrie->find(keyword_lower);
  if (type != TokenType::INVALID) {
    std::string name = getTokenTypeName(type);
    return {type, keyword_lower, name, line, column};
  }
  if (isImmediate(keyword_lower)) {
    return Token{
        TokenType::IMMEDIATE, keyword_lower, "IMMEDIATE", line, column};
  }
  return Token{TokenType::VALUE, keyword, "VALUE", line, column};
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

// Read an immediate value from the input string
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

  return Token{TokenType::IMMEDIATE, value, "IMMEDIATE", line, startColumn};
}

// Read a punctuation character from the input string
Token Lexer::readPunctuation() {
  char c = advance();
  return {
      TokenType::PUNCTUATION, std::string(1, c), "PUNCTUATION", line, column};
}

// Read a label from the input string
Token Lexer::readLabel() {
  std::string value;
  if (peek() == '.') {
    value += advance();
  }
  while (position < source.size() && isalnum(source[position])) {
    value += advance();
  }
  if (peek() == ':') {
    advance(); // Consume ':'
    return {TokenType::LABEL, value, "LABEL", line, column};
  } else if (peek() == ' ') {
    skipWhitespace();
  } else if (peek() == '[') {
    std::string addressing_mode;
    addressing_mode += advance(); // Consume '['
    while (peek() != ']' && peek() != '\0') {
      addressing_mode += advance();
    }
    if (peek() == ']') {
      addressing_mode += advance(); // Consume ']'
    } else {
      reportError("Unterminated addressing mode in label: " + value);
    }
    return {
        TokenType::INSTRUCTION,
        value + addressing_mode,
        "INSTRUCTION",
        line,
        column};
  }
  return {TokenType::INVALID, value, "INVALID", line, column};
}

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
  // handle values like .-main
  if (lower.find('.') != std::string::npos) {
    return {TokenType::VALUE, lower, "VALUE", line, column};
  } else {
    reportError("Unknown directive: " + lower);
  }
  return {TokenType::INVALID, lower, "INVALID", line, column};
}

// Read a string for some cases we have strings in the code
Token Lexer::readString() {
  std::string value;
  // the string is started with a '"'
  // so we need to add it to the value
  value += advance();
  while (peek() != '"') {
    value += advance();
  }
  // the string is ended with a '"'
  // so we need to add it to the value
  value += advance();
  return {TokenType::STRING, value, "STRING", line, column};
}

// Tokenize the input string
std::vector<Token> Lexer::tokenize() {
  std::vector<Token> tokens;

  while (position < source.size()) {
    skipWhitespace();
    skipComment();

    if (position >= source.size())
      break;

    char c = peek();

    if (isalnum(c) || c == '.') {
      // check if it is either a directive or label as some
      // labels start with a '.'
      size_t nextWhitespace = source.find_first_of(" \t\n", position);
      size_t nextColon = source.find(':', position);
      if (c == '.') {
        size_t nextWhitespace = source.find_first_of(" \t\n", position);
        size_t nextColon = source.find(':', position);
        size_t nextBracket = source.find('[', position);
        if (nextColon != std::string::npos && nextColon < nextWhitespace) {
          tokens.push_back(readLabel());
        } else if (
            nextBracket != std::string::npos && nextBracket < nextWhitespace) {
          tokens.push_back(readLabel());
        } else {
          tokens.push_back(readDirective());
        }
      } else if (nextColon != std::string::npos && nextColon < nextWhitespace) {
        tokens.push_back(readLabel());
        // tokens.push_back(readAddressingMode());
      } else {
        tokens.push_back(readKeyword());
      }
    } else if (c == '"') {
      tokens.push_back(readString());
    } else if (
        isdigit(c) ||
        (c == '0' &&
             (source[position + 1] == 'x' || source[position + 1] == 'X') ||
         (c == '-' && isdigit(source[position + 1])) ||
         (c == '-' && isdigit(source[position + 1])))) {
      tokens.push_back(readImmediate());
    } else {
      tokens.push_back(readPunctuation());
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