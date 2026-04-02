#ifndef CODEGEN_H
#define CODEGEN_H

#include <cstddef>
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils.h"
#include "llvm_ir/LLVMIRGenerator.h"

enum class OptLevel { O0, O1, O2 };

// TargetConfig — fully describes a code-generation target.
//
// Predefined configs are provided as static constants.  To add a new target,
// add an entry to the TARGETS table in main.cpp, initialize its backend in
// initializeTarget() in Codegen.cpp, and implement its syscall convention in
// SyscallBuilder::emitSyscall() in LLVMIRGenerator.cpp.
struct TargetConfig {
  std::string name; // human-readable: "macos-arm64"
  std::string triple; // LLVM target triple
  std::string backend; // LLVM backend family: "AArch64" | "X86" | "RISCV"
  TargetABI abi; // syscall ABI selector

  // Predefined targets ───────────────────────────────────────────────────
  static const TargetConfig MACOS_ARM64; // default
  static const TargetConfig LINUX_ARM64;
  static const TargetConfig LINUX_X86_64;
  // TODO: LINUX_RISCV64 will be added once SyscallBuilder implements the RISC-V
  // ABI.

  static const TargetConfig& defaultTarget() {
    return MACOS_ARM64;
  }
};

class Codegen {
 public:
  /// Construct with an explicit target configuration (default: macOS ARM64).
  explicit Codegen(const TargetConfig& config = TargetConfig::defaultTarget());

  /// Run LLVM optimization passes on the module at the given level.
  /// O0 = no optimization (raw lifted IR), O1 = mem2reg + basic cleanup,
  /// O2 = full LLVM default pipeline.
  void optimize(llvm::Module& module, OptLevel level);

  /// Emit assembly for the configured target into outputFilename.
  void generateAssembly(
      llvm::Module& module,
      const std::string& outputFilename);

  const TargetConfig& target() const {
    return config_;
  }

 private:
  TargetConfig config_;
};

#endif // CODEGEN_H