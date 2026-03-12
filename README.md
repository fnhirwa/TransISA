# TransISA

A static transpiler that converts x86 assembly source code to semantically equivalent ARMv8-A (AArch64) assembly through LLVM IR.

```
x86 Assembly (.s) → Lexer → Parser → AST → LLVM IR → [Optimization] → ARM Assembly (.s)
```

TransISA uses traditional compiler techniques to give developers full transparency and control over the translation process. Every intermediate stage (tokens, AST, IR, optimized IR) is inspectable, making the tool suitable for legacy code migration, compiler research, and cross-ISA analysis.

## Features

- **Full pipeline**: Lexer → Parser → AST → LLVM IR → ARM assembly in a single invocation
- **Configurable optimization**: Three levels (O0, O1, O2) to control the fidelity-vs-efficiency tradeoff
- **IR inspection**: Dump LLVM IR before and after optimization with `--emit-ir`
- **Cross-platform**: Builds and runs on macOS (Apple Silicon / Intel) and Linux
- **Tested**: Unit tests for lexer, parser, and IR generation via Google Test; CI on both platforms

### Supported x86 Instructions

| Category | Instructions |
|---|---|
| Data movement | `mov`, `lea`, `push`, `pop` |
| Arithmetic | `add`, `sub`, `mul`, `div`, `inc`, `dec`, `neg` |
| Bitwise | `and`, `or`, `xor`, `shl`, `shr`, `sar` |
| Comparison | `cmp`, `test` |
| Control flow | `jmp`, `je`, `jne`, `jg`, `jge`, `jl`, `jle`, `ja`, `jae`, `jb`, `jbe`, `jz`, `jnz`, `jo`, `jno`, `js`, `jns` |
| Procedure | `call`, `ret` |
| System | `syscall`, `int` |
| Directives | `.text`, `.data`, `.bss`, `.global`, `.asciz`, `.byte`, `.word`, `.long`, `.quad`, `resb`, `resw`, `resd` |

## Prerequisites

- **CMake** >= 3.16
- **LLVM 19** (with AArch64 and X86 backends)
- **C++17** compiler (Clang recommended)
- **Ninja** (optional, recommended)

### Installing LLVM 19

**macOS (Homebrew):**
```bash
brew install llvm@19 cmake ninja
```

**Ubuntu/Debian:**
```bash
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
sudo add-apt-repository "deb http://apt.llvm.org/jammy/ llvm-toolchain-jammy-19 main"
sudo apt-get update
sudo apt-get install -y cmake ninja-build llvm-19 llvm-19-dev clang-19 lld-19
```

## Building

```bash
git clone https://github.com/fnhirwa/TransISA.git
cd TransISA

# Configure
cmake -B build -S . -G Ninja -DCMAKE_BUILD_TYPE=Release

# Build
cmake --build build

# Verify
./build/src/TransISA --help
```

If CMake cannot find LLVM, set the path manually:
```bash
# macOS
cmake -B build -S . -G Ninja -DLLVM_DIR=/opt/homebrew/opt/llvm@19/lib/cmake/llvm

# Linux
cmake -B build -S . -G Ninja -DLLVM_DIR=/usr/lib/llvm-19/lib/cmake/llvm
```

## Usage

```
Usage: TransISA <input.s> [options]

Options:
  -o <file>       Output assembly file (default: output.s)
  --opt-level=N   Optimization level: 0, 1, 2 (default: 0)
  --emit-ir       Dump LLVM IR to stderr before and after optimization
  --verbose       Print tokens, AST, and pipeline stages
  --help          Show this message
```

### Basic transpilation

```bash
# Transpile x86 to ARM with no optimization (raw lifted output)
./build/src/TransISA benchmarking/x86/hello.s -o hello_arm.s --opt-level=0

# With LLVM O2 optimization
./build/src/TransISA benchmarking/x86/hello.s -o hello_arm_O2.s --opt-level=2

# Inspect the LLVM IR at each stage
./build/src/TransISA benchmarking/x86/add.s -o add_arm.s --opt-level=2 --emit-ir 2> add_ir.ll
```

### Building and running the output on ARM (macOS Apple Silicon)

```bash
# Transpile
./build/src/TransISA benchmarking/x86/hello.s -o hello_arm.s --opt-level=2

# Assemble and link
as -arch arm64 hello_arm.s -o hello_arm.o
ld -macosx_version_min 11.0 -o hello_arm hello_arm.o \
   -lSystem -syslibroot $(xcrun --show-sdk-path) -e __start

# Run
./hello_arm
```

### Comparing optimization levels

Run a benchmark at all three levels to see the impact of LLVM passes:

```bash
for level in 0 1 2; do
  ./build/src/TransISA benchmarking/x86/add.s \
    -o benchmarking/arm/add_O${level}.s \
    --opt-level=${level}
done
```

## Benchmarking

The `benchmarking/` directory contains x86 source programs and a Python script that automates metric collection.

```bash
cd benchmarking
python3 analyzefiles.py
```

This compiles both x86 and ARM assembly files, links them, and prints a comparison table with:
- Instruction count
- Text section size (bytes)
- Syscall count
- Stack size (bytes)

**Requirements**: macOS with both x86_64 and arm64 toolchains available (standard on Apple Silicon Macs). Requires `llvm-objdump` and `otool`.

## Running Tests

```bash
cd build
ctest --output-on-failure
```

Or run individual test suites:

```bash
./build/tests/test_lexer
./build/tests/test_parser
./build/tests/test_llvm_codegen
```

## Project Structure

```
TransISA/
├── include/
│   ├── lexer/           # Lexer, Token, Trie headers
│   ├── parser/          # Parser, AST node definitions
│   ├── llvm_ir/         # LLVM IR generator header
│   └── codegen/         # Backend codegen + optimization header
├── src/
│   ├── lexer/           # Trie-based tokenizer
│   ├── parser/          # AST construction from token stream
│   ├── llvm_ir/         # x86 → LLVM IR mapping (1500+ lines)
│   ├── codegen/         # LLVM optimization passes + ARM emission
│   └── main.cpp         # CLI entry point
├── tests/               # Google Test suites (lexer, parser, IR gen)
├── benchmarking/
│   ├── x86/             # Source x86 assembly programs
│   ├── arm/             # Transpiled ARM output
│   └── analyzefiles.py  # Automated metric collection
├── .github/workflows/   # CI: tests (macOS + Ubuntu) + code quality
├── CMakeLists.txt
└── LICENSE              # MIT
```

## How It Works

1. **Lexer**: Tokenizes x86 assembly using a trie-based classifier. Handles registers, instructions, immediates (decimal/hex/binary/octal), labels, directives, memory operands, and strings.

2. **Parser**: Constructs a hierarchical AST: `RootNode` → `FunctionNode` → `BasicBlockNode` → `InstructionNode` with typed operands (`RegisterNode`, `IntLiteralNode`, `MemoryNode`, etc.).

3. **IR Generator**: Walks the AST and emits LLVM IR. x86 registers are modeled as `alloca` variables. CPU flags (ZF, SF, CF, OF) are explicitly tracked. Stack operations use a simulated memory buffer with pointer arithmetic. Syscalls are translated to platform-appropriate inline assembly.

4. **Optimizer**: Applies LLVM's pass pipeline at the requested level. The `mem2reg` pass (active at O1+) promotes register allocas to SSA values, which is the most impactful transformation for transpiled code quality.

5. **Backend**: Feeds optimized IR to LLVM's AArch64 code generator for register allocation, instruction selection, and assembly emission.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md). The project uses `clang-format` and `cmake-format` via pre-commit hooks.

```bash
pip install pre-commit
pre-commit install
```

## Citation

If you use TransISA in your research, please cite:

```bibtex
@inproceedings{nshuti2026transisa,
  author    = {Nshuti, Felix Hirwa},
  title     = {{TransISA}: A Static Transpiler for Migrating Legacy x86 Assembly to {ARM} in Scientific Computing},
  booktitle = {Proceedings of the 2026 Improving Scientific Software Conference (ISS26)},
  year      = {2026},
  address   = {Boulder, CO, USA}
}
```

## License

[MIT](LICENSE)