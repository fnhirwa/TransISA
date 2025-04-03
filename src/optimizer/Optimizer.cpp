#include "llvm/IR/LegacyPassManager.h"
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

#include "optimizer/Optimizer.h"

Optimizer::Optimizer() {
  // Initialize LLVM targets
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();
}

void Optimizer::optimize(llvm::Module& module) {
  // Create a pass manager
  llvm::legacy::PassManager passManager;

  // Add optimization passes
  passManager.add(llvm::createPromoteMemoryToRegisterPass()); // Example pass
  passManager.add(llvm::createInstructionCombiningPass()); // Example pass
  passManager.add(llvm::createDeadCodeEliminationPass()); // Example pass

  // Run the passes
  passManager.run(module);
}

void Optimizer::generateAssembly(
    llvm::Module& module,
    const std::string& outputFilename) {
  // Open the output file
  std::error_code errorCode;
  llvm::raw_fd_ostream outputStream(
      outputFilename, errorCode, llvm::sys::fs::OF_None);
  if (errorCode) {
    llvm::errs() << "Error opening output file: " << errorCode.message()
                 << "\n";
    return;
  }

  // Get the target machine (e.g., x86)
  std::string targetTriple = llvm::sys::getDefaultTargetTriple();
  std::string error;
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(targetTriple, error);
  if (!target) {
    llvm::errs() << "Error finding target: " << error << "\n";
    return;
  }

  // Configure the target machine
  llvm::TargetOptions targetOptions;
  llvm::TargetMachine* targetMachine = target->createTargetMachine(
      targetTriple, "generic", "", targetOptions, llvm::Reloc::Model::PIC_);

  // Set the module's target triple
  module.setTargetTriple(targetTriple);

  // Generate assembly code
  llvm::legacy::PassManager passManager;
  targetMachine->addPassesToEmitFile(
      passManager, outputStream, nullptr, llvm::CodeGenFileType::AssemblyFile);

  // Run the passes
  passManager.run(module);

  // Clean up
  outputStream.flush();
}
