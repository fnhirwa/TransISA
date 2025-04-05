#ifndef CODEGEN_H
#define CODEGEN_H

#include <cstddef>
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
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

class Codegen {
 public:
  Codegen();
  void optimize(llvm::Module& module);
  void generateAssembly(
      llvm::Module& module,
      const std::string& outputFilename);
};

#endif // CODEGEN_H