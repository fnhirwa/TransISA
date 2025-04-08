#include "codegen/Codegen.h"

void Codegen::optimize(llvm::Module& module) {
  // Create a pass manager
  llvm::legacy::PassManager passManager;

  // Add optimization passes
  passManager.add(llvm::createPromoteMemoryToRegisterPass()); // Example pass
  passManager.add(llvm::createInstructionCombiningPass()); // Example pass
  passManager.add(llvm::createDeadCodeEliminationPass()); // Example pass

  // Run the passes
  passManager.run(module);
}

std::string detectHostTarget() {
#if defined(__aarch64__) || defined(_M_ARM64)
  return "AArch64";
#elif defined(__x86_64__) || defined(_M_X64)
  return "X86";
#else
  throw std::runtime_error("Unknown or unsupported architecture.");
#endif
}

void initializeTarget(const std::string& target) {
  if (target == "AArch64") {
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64AsmParser();
    LLVMInitializeAArch64AsmPrinter();
  } else if (target == "X86") {
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmParser();
    LLVMInitializeX86AsmPrinter();
  } else {
    throw std::runtime_error("Unsupported target: " + target);
  }
}

Codegen::Codegen(const std::string& targetOverride) {
  std::string target =
      targetOverride.empty() ? detectHostTarget() : targetOverride;
  initializeTarget(target);
}

void Codegen::generateAssembly(
    llvm::Module& module,
    const std::string& outputFilename) {
  // Open output file
  std::error_code errorCode;
  llvm::raw_fd_ostream outputStream(
      outputFilename, errorCode, llvm::sys::fs::OF_None);
  if (errorCode) {
    llvm::errs() << "Error opening output file: " << errorCode.message()
                 << "\n";
    return;
  }

  // Setup target triple
  std::string targetTriple = llvm::sys::getDefaultTargetTriple();
  module.setTargetTriple(targetTriple);

  std::string error;
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(targetTriple, error);
  if (!target) {
    llvm::errs() << "Error finding target: " << error << "\n";
    return;
  }

  // Create target machine
  llvm::TargetOptions targetOptions;
  auto* targetMachine = target->createTargetMachine(
      targetTriple, "generic", "", targetOptions, llvm::Reloc::Model::PIC_);
  module.setDataLayout(targetMachine->createDataLayout());

  // Emit assembly
  llvm::legacy::PassManager passManager;
  if (targetMachine->addPassesToEmitFile(
          passManager,
          outputStream,
          nullptr,
          llvm::CodeGenFileType::AssemblyFile)) {
    llvm::errs() << "TargetMachine can't emit an assembly file.\n";
    return;
  }
  passManager.run(module);
  outputStream.flush();
}