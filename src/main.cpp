#include <cstring>
#include <iostream>
#include <unordered_map>
#include <vector>
#include "codegen/Codegen.h"
#include "lexer/Lexer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm_ir/LLVMIRGenerator.h"
#include "parser/Parser.h"

using namespace std;

struct CliOptions {
  std::string inputFile;
  std::string outputFile = "output.s";
  OptLevel optLevel = OptLevel::O0;
  bool emitIR = false;
  bool verbose = false;
  // Target selection — defaults to macOS ARM64 (the primary benchmark
  // platform). Override with --target=<name>; see TARGETS table below.
  std::string targetName = "macos-arm64";
};

// Supported targets.  To add a new one:
//  Add a TargetConfig constant in Codegen.h / Codegen.cpp.
//  Implement its syscall ABI in SyscallBuilder::emitSyscall().
//  Insert it here.
static const std::unordered_map<std::string, const TargetConfig*> TARGETS = {
    {"macos-arm64", &TargetConfig::MACOS_ARM64},
    {"linux-arm64", &TargetConfig::LINUX_ARM64},
    {"linux-x86_64", &TargetConfig::LINUX_X86_64},
};

void printUsage(const char* progName) {
  std::cerr
      << "Usage: " << progName << " <input.s> [options]\n"
      << "Options:\n"
      << "  -o <file>           Output assembly file (default: output.s)\n"
      << "  --opt-level=N       Optimization level: 0, 1, 2 (default: 0)\n"
      << "  --target=<name>     Code-generation target (default: macos-arm64)\n"
      << "                      Supported: macos-arm64, linux-arm64, "
         "linux-x86_64\n"
      << "  --emit-ir           Dump LLVM IR to stderr before codegen\n"
      << "  --verbose           Print tokens, AST, and pipeline stages\n"
      << "  --help              Show this message\n";
}

CliOptions parseArgs(int argc, char* argv[]) {
  CliOptions opts;
  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "--help" || arg == "-h") {
      printUsage(argv[0]);
      exit(0);
    } else if (arg == "-o" && i + 1 < argc) {
      opts.outputFile = argv[++i];
    } else if (arg.rfind("--opt-level=", 0) == 0) {
      int level = std::stoi(arg.substr(12));
      if (level == 0)
        opts.optLevel = OptLevel::O0;
      else if (level == 1)
        opts.optLevel = OptLevel::O1;
      else if (level == 2)
        opts.optLevel = OptLevel::O2;
      else {
        std::cerr << "Error: invalid optimization level (use 0, 1, or 2)\n";
        exit(1);
      }
    } else if (arg.rfind("--target=", 0) == 0) {
      opts.targetName = arg.substr(9);
      if (TARGETS.find(opts.targetName) == TARGETS.end()) {
        std::cerr
            << "Error: unknown target '" << opts.targetName << "'\n"
            << "Supported targets: macos-arm64, linux-arm64, linux-x86_64\n";
        exit(1);
      }
    } else if (arg == "--emit-ir") {
      opts.emitIR = true;
    } else if (arg == "--verbose") {
      opts.verbose = true;
    } else if (arg[0] != '-') {
      opts.inputFile = arg;
    } else {
      std::cerr << "Unknown option: " << arg << "\n";
      printUsage(argv[0]);
      exit(1);
    }
  }
  if (opts.inputFile.empty()) {
    std::cerr << "Error: no input file specified\n";
    printUsage(argv[0]);
    exit(1);
  }
  return opts;
}

int main(int argc, char* argv[]) {
  CliOptions opts = parseArgs(argc, argv);

  std::string bufferStorage;
  try {
    std::string_view source =
        readFileToStringView(opts.inputFile, bufferStorage);

    // Lexical Analysis
    Lexer lexer(source);
    std::vector<Token> tokens = lexer.tokenize();
    if (opts.verbose) {
      std::cout << "=== Tokens ===\n";
      for (const Token& token : tokens) {
        std::cout << token.value << " " << token.type_name << " " << token.line
                  << " " << token.column << std::endl;
      }
    }

    // Parsing to AST
    Parser parser(tokens);
    std::unique_ptr<ASTNode> root = parser.parse();
    if (opts.verbose) {
      std::cout << "\n=== AST ===\n";
      root->print();
    }

    // Resolve target configuration once — shared by IR generator and codegen.
    const TargetConfig& targetCfg = *TARGETS.at(opts.targetName);

    // LLVM IR Generation
    LLVMIRGen irGen(targetCfg.abi);
    llvm::Module* module = irGen.generateIR(root);

    if (opts.emitIR) {
      std::cerr << "\n=== LLVM IR (before optimization) ===\n";
      module->print(llvm::errs(), nullptr);
    }

    // Optimization (configurable)
    Codegen codegen(targetCfg);
    codegen.optimize(*module, opts.optLevel);

    if (opts.emitIR && opts.optLevel != OptLevel::O0) {
      std::cerr << "\n=== LLVM IR (after O" << static_cast<int>(opts.optLevel)
                << ") ===\n";
      module->print(llvm::errs(), nullptr);
    }

    // Assembly Generation
    std::cout << "Generating assembly -> " << opts.outputFile << "\n";
    codegen.generateAssembly(*module, opts.outputFile);
    std::cout << "Done.\n";

  } catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << '\n';
    return 1;
  }

  return 0;
}