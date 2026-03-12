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

class Codegen {
 public:
  Codegen(const std::string& targetOverride = "");

  /// Run LLVM optimization passes on the module at the given level.
  /// O0 = no optimization (raw lifted IR), O1 = mem2reg + basic cleanup,
  /// O2 = full LLVM default pipeline.
  void optimize(llvm::Module& module, OptLevel level);

  void generateAssembly(
      llvm::Module& module,
      const std::string& outputFilename);
};

#endif // CODEGEN_H