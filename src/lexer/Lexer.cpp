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
  preprocessedSource_ = preprocess(std::string(source));
  this->source = preprocessedSource_;
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

  // some more directives
  // In Lexer constructor, with your existing DIRECTIVE inserts:
  keywordTrie->insert(".att_syntax", TokenType::DIRECTIVE);
  keywordTrie->insert(".intel_syntax", TokenType::DIRECTIVE);
  keywordTrie->insert(".p2align", TokenType::DIRECTIVE);
  keywordTrie->insert(".balign", TokenType::DIRECTIVE);
  keywordTrie->insert(".weak", TokenType::DIRECTIVE);
  keywordTrie->insert(".comm", TokenType::DIRECTIVE);
  keywordTrie->insert(".lcomm", TokenType::DIRECTIVE);
  keywordTrie->insert(".set", TokenType::DIRECTIVE);
  keywordTrie->insert(".equ", TokenType::DIRECTIVE);
  keywordTrie->insert(".hidden", TokenType::DIRECTIVE);
  keywordTrie->insert(".protected", TokenType::DIRECTIVE);
  keywordTrie->insert(".skip", TokenType::DIRECTIVE);
  keywordTrie->insert(".space", TokenType::DIRECTIVE);
}
// Some of the directives carry no semantic value during IR building
static const std::unordered_set<std::string> discardablePrefixes = {
    ".cfi_", // all CFI unwind directives
    ".loc", // debug location
    ".loc\t",
    ".file", // debug file table
    ".file\t",
    ".ident", // compiler ident string
    ".addrsig", // address significance table (LLD)
    "Lfunc_end", // local end-of-function labels emitted by clang
    ".type", // GAS symbol type annotation (.type sym, @function)
};

// GAS mnemonics size suffixes to strip
// only strip when the suffix is LAST char and the base is known to be valid.
// using a simple char check: b/w/l/q/s/d at the end of the mnemonic.
static const std::unordered_set<std::string> knownMnemonics = {
    "mov", "add",  "sub",  "imul", "idiv", "and", "or",   "xor", "not", "neg",
    "cmp", "test", "push", "pop",  "lea",  "ret", "call", "jmp", "je",  "jne",
    "jl",  "jle",  "jg",   "jge",  "jz",   "jnz", "shl",  "shr", "sar", "sal",
    "inc", "dec",  "movs", "movz", "cbw",  "cwd", "cdq",  "cqo",
};

// top level entry point for preprocessing
std::string Lexer::preprocess(const std::string& raw) {
  detectedMode_ = detectSyntax(raw);
  bool isATT = (detectedMode_ == SyntaxMode::ATT);

  std::ostringstream out;
  std::istringstream in(raw);
  std::string line;
  while (std::getline(in, line)) {
    std::string result = processLine(line, isATT);
    if (!result.empty())
      out << result << '\n';
  }
  return out.str();
}

SyntaxMode Lexer::detectSyntax(const std::string& raw) {
  if (raw.find(".intel_syntax") != std::string::npos)
    return SyntaxMode::Intel;
  if (raw.find(".att_syntax") != std::string::npos)
    return SyntaxMode::ATT;

  // heuristic: count lines with '%' (AT&T registers) vs '[' (Intel memory)
  int attVotes = 0, intelVotes = 0;
  std::istringstream ss(raw);
  std::string line;
  while (std::getline(ss, line)) {
    if (line.find('%') != std::string::npos)
      ++attVotes;
    if (line.find('[') != std::string::npos)
      ++intelVotes;
  }
  return (attVotes > intelVotes) ? SyntaxMode::ATT : SyntaxMode::Intel;
}

bool Lexer::isDiscardableDirective(const std::string& line) {
  for (const auto& prefix : discardablePrefixes)
    if (line.rfind(prefix, 0) == 0)
      return true;
  return false;
}

std::string Lexer::processLine(const std::string& line, bool isATT) {
  // trim leading whitespace for prefix checks
  size_t firstNonSpace = line.find_first_not_of(" \t");
  if (firstNonSpace == std::string::npos)
    return ""; // blank line → discard

  std::string trimmed = line.substr(firstNonSpace);

  // discard full-line comments
  if (trimmed[0] == '#' || trimmed[0] == ';')
    return "";

  // discard semantically irrelevant directives
  if (isDiscardableDirective(trimmed))
    return "";

  if (!isATT) {
    // intel mode: only normalize ';' comments (already correct)
    // strip inline # comments that GAS sometimes emits even in intel mode
    size_t hashPos = trimmed.find('#');
    if (hashPos != std::string::npos)
      trimmed = trimmed.substr(0, hashPos);
    // trim trailing whitespace
    size_t last = trimmed.find_last_not_of(" \t");
    return (last != std::string::npos) ? trimmed.substr(0, last + 1) : "";
  }

  // ATT normalization pipeline:
  // strip inline # comments first
  size_t hashPos = trimmed.find('#');
  if (hashPos != std::string::npos)
    trimmed = trimmed.substr(0, hashPos);

  // trim trailing whitespace
  size_t last = trimmed.find_last_not_of(" \t");
  if (last != std::string::npos)
    trimmed = trimmed.substr(0, last + 1);
  if (trimmed.empty())
    return "";

  // if this is a label line (ends with ':'), pass through unchanged
  if (trimmed.back() == ':')
    return trimmed;

  // if this is a directive line (starts with '.'), pass through
  //    (segment directives like .text, .data, .globl etc. stay as-is)
  if (trimmed[0] == '.')
    return trimmed;

  // split into mnemonic + operand string
  size_t spacePos = trimmed.find_first_of(" \t");
  std::string mnemonic = trimmed.substr(0, spacePos);
  std::string operands =
      (spacePos != std::string::npos) ? trimmed.substr(spacePos + 1) : "";

  // trim operands leading whitespace
  size_t opStart = operands.find_first_not_of(" \t");
  if (opStart != std::string::npos)
    operands = operands.substr(opStart);

  // strip size suffix from mnemonic (movl -> mov, addq -> add)
  mnemonic = stripSizeSuffix(mnemonic);

  // normalize operands (strip %, $, convert memory, reverse order)
  std::string normalizedOps = normalizeOperandList(mnemonic, operands);

  if (normalizedOps.empty())
    return mnemonic;
  return mnemonic + " " + normalizedOps;
}

// GAS suffix stripping logic: only strip if the base mnemonic is known to be
// valid
std::string Lexer::stripSizeSuffix(const std::string& mnemonic) {
  if (mnemonic.size() < 2)
    return mnemonic;

  char last = mnemonic.back();
  // valid GAS suffixes: b(byte) w(word) l(long/32) q(quad/64) s(float)
  // d(double)
  if (last != 'b' && last != 'w' && last != 'l' && last != 'q' && last != 's' &&
      last != 'd')
    return mnemonic;

  std::string base = mnemonic.substr(0, mnemonic.size() - 1);
  if (knownMnemonics.count(base))
    return base;

  // handle two-char suffixes like movzbl, movsbq
  if (mnemonic.size() >= 3) {
    char secondLast = mnemonic[mnemonic.size() - 2];
    if ((secondLast == 'b' || secondLast == 'w' || secondLast == 'l') &&
        (last == 'l' || last == 'q')) {
      std::string base2 = mnemonic.substr(0, mnemonic.size() - 2);
      if (knownMnemonics.count(base2))
        return base2;
    }
  }
  return mnemonic;
}

// convert AT&T memory operand to Intel bracket notation
// AT&T forms:
//   (%rbp)          -> [rbp]
//   -8(%rbp)        -> [rbp-8]
//   8(%rbp)         -> [rbp+8]
//   (%rax,%rbx,4)   -> [rax+rbx*4]
//   -4(%rax,%rbx,2) -> [rax+rbx*2-4]
std::string Lexer::convertMemoryOperand(const std::string& op) {
  // must contain '(' to be a memory operand
  size_t lp = op.find('(');
  if (lp == std::string::npos)
    return op;

  size_t rp = op.find(')', lp);
  if (rp == std::string::npos)
    return op;

  std::string disp = op.substr(0, lp); // displacement before '('
  std::string inner = op.substr(lp + 1, rp - lp - 1); // inside parens

  // strip '%' from everything inside
  std::string cleanInner;
  for (char c : inner)
    if (c != '%')
      cleanInner += c;

  // parse inner: base, index, scale separated by commas
  std::vector<std::string> parts;
  std::istringstream ss(cleanInner);
  std::string part;
  while (std::getline(ss, part, ',')) {
    // trim
    size_t s = part.find_first_not_of(" \t");
    size_t e = part.find_last_not_of(" \t");
    if (s != std::string::npos)
      parts.push_back(part.substr(s, e - s + 1));
  }

  std::string result = "[";
  if (!parts.empty())
    result += parts[0]; // base
  if (parts.size() >= 2 && !parts[1].empty()) {
    result += "+" + parts[1]; // index
    if (parts.size() >= 3 && !parts[2].empty() && parts[2] != "1")
      result += "*" + parts[2]; // scale (omit *1)
  }
  // displacement
  if (!disp.empty()) {
    // disp already has sign (e.g. "-8" or "8")
    if (disp[0] == '-')
      result += disp; // [rbp-8]
    else
      result += "+" + disp; // [rbp+8]
  }
  result += "]";
  return result;
}

// normalize a full operand list: split on commas, normalize each operand, then
// reverse order for 2-operand instructions
std::string Lexer::normalizeOperandList(
    const std::string& mnemonic,
    const std::string& operands) {
  if (operands.empty())
    return "";

  // split operands on ',' but not inside '(' ')'
  std::vector<std::string> ops;
  std::string current;
  int depth = 0;
  for (char c : operands) {
    if (c == '(')
      ++depth;
    else if (c == ')')
      --depth;
    if (c == ',' && depth == 0) {
      ops.push_back(current);
      current.clear();
    } else {
      current += c;
    }
  }
  if (!current.empty())
    ops.push_back(current);

  // normalize each operand
  for (auto& op : ops) {
    // trim
    size_t s = op.find_first_not_of(" \t");
    size_t e = op.find_last_not_of(" \t");
    if (s == std::string::npos) {
      op = "";
      continue;
    }
    op = op.substr(s, e - s + 1);

    if (op[0] == '%') {
      // register: strip '%'
      op = op.substr(1);
    } else if (op[0] == '$') {
      // immediate: strip '$'
      op = op.substr(1);
    } else if (op.find('(') != std::string::npos) {
      // memory operand
      op = convertMemoryOperand(op);
    }
    // else: label/identifier — leave as-is
  }

  // reverse operand order for AT&T → Intel (src,dst → dst,src)
  // but only for 2-operand instructions. Single operand (push, pop, jmp,
  // call, inc, dec, not, neg) keep their order.
  if (ops.size() == 2) {
    std::reverse(ops.begin(), ops.end());
  }

  // rejoin
  std::string result;
  for (size_t i = 0; i < ops.size(); ++i) {
    if (i > 0)
      result += ", ";
    result += ops[i];
  }
  return result;
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
      position++; // advance past '\n' without calling advance() to avoid
                  // incrementing column
    } else {
      advance();
    }
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

  // Overwrite the token's line/column with the position captured just BEFORE
  // the read function consumed any characters (start-of-token, not end).
  auto withStartPos = [&](Token t) -> Token {
    t.line = tokenStartLine_;
    t.column = tokenStartCol_;
    return t;
  };

  while (position < source.size()) {
    skipWhitespace(); // Then skip remaining spaces
    while (peek() == ';') { // Ensure all comments are skipped
      skipComment();
      skipWhitespace(); // Also clear trailing spaces after a comment
    }
    if (position >= source.size())
      break;
    tokenStartLine_ = line;
    tokenStartCol_ = column + 1;
    char c = peek();

    if (isalnum(c) || c == '.' || c == '_') {
      size_t nextWhitespace = source.find_first_of(" \t\n", position);
      size_t nextColon = source.find(':', position);

      if (c == '.' || "_") {
        size_t nextBracket = source.find('[', position);
        if (nextColon != std::string::npos && nextColon < nextWhitespace) {
          tokens.push_back(withStartPos(readLabel()));
        } else if (
            nextBracket != std::string::npos && nextBracket < nextWhitespace) {
          tokens.push_back(withStartPos(readLabel()));
        } else {
          if (c == '.' || c == '_') {
            tokens.push_back(withStartPos(readDirective()));
          } else {
            tokens.push_back(withStartPos(readValue()));
          }
        }
      } else if (nextColon != std::string::npos && nextColon < nextWhitespace) {
        tokens.push_back(withStartPos(readLabel()));
      } else {
        tokens.push_back(withStartPos(readValue()));
      }
    } else if (c == '"' || c == '\'') {
      tokens.push_back(withStartPos(readString()));
    } else if (
        isdigit(c) ||
        (c == '0' && position + 1 < source.size() &&
         (source[position + 1] == 'x' || source[position + 1] == 'X' ||
          source[position + 1] == 'b' || source[position + 1] == 'o' ||
          source[position + 1] == 'O' || source[position + 1] == 'q')) ||
        (c == '-' && position + 1 < source.size() &&
         isdigit(source[position + 1]))) {
      tokens.push_back(withStartPos(readImmediate()));
    } else if (c == ',') {
      tokens.push_back(withStartPos(readComma()));
    } else if (c == ':') {
      tokens.push_back(withStartPos(readColon()));
    } else if (c == '[') {
      tokens.push_back(withStartPos(readLeftBracket()));
    } else if (c == ']') {
      tokens.push_back(withStartPos(readRightBracket()));
    } else if (c == ';') {
      skipComment();
    } else if (
        std::find(
            punctuations.begin(), punctuations.end(), std::string(1, c)) !=
        punctuations.end()) {
      tokens.push_back(withStartPos(readPunctuation()));
    } else {
      reportError("Unexpected character: " + std::string(1, c));
    }
  }

  // Place END on the same line as the last real token (not the line after a
  // trailing newline).
  size_t endLine = tokens.empty() ? line : tokens.back().line;
  tokens.push_back({TokenType::END, "END", "END", endLine, column});
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