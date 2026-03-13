#include "codegen/Codegen.h"

void Codegen::optimize(llvm::Module& module, OptLevel level) {
  if (level == OptLevel::O0) {
    // No optimization — emit raw lifted IR as-is
    return;
  }

  llvm::PassBuilder PB;
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::OptimizationLevel optLevel;
  switch (level) {
    case OptLevel::O1:
      optLevel = llvm::OptimizationLevel::O1;
      break;
    case OptLevel::O2:
      optLevel = llvm::OptimizationLevel::O2;
      break;
    default:
      return;
  }

  llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(optLevel);
  MPM.run(module, MAM);
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
  // TransISA is always a cross-compiler: output is always ARM64.
  // Always make sure the AArch64 backend is initialised regardless of host.
  if (target != "AArch64") {
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64AsmParser();
    LLVMInitializeAArch64AsmPrinter();
  }
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

  // TransISA always targets Apple Silicon macOS — even when built on a Linux
  // x86-64 host (e.g. WSL).  The inline asm uses ARM64 instructions (svc
  // #0x80) which are only valid for an AArch64 target.  Using the host triple
  // on x86-64 makes LLVM reject them.
  const std::string targetTriple = "aarch64-apple-macosx11.0.0";
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
