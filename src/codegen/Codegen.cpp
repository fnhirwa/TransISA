#include "codegen/Codegen.h"

// Static TargetConfig definitions
const TargetConfig TargetConfig::MACOS_ARM64 = {
    "macos-arm64",
    "aarch64-apple-macosx11.0.0",
    "AArch64",
    TargetABI::MacOS_ARM64,
};
const TargetConfig TargetConfig::LINUX_ARM64 = {
    "linux-arm64",
    "aarch64-unknown-linux-gnu",
    "AArch64",
    TargetABI::Linux_ARM64,
};
const TargetConfig TargetConfig::LINUX_X86_64 = {
    "linux-x86_64",
    "x86_64-unknown-linux-gnu",
    "X86",
    TargetABI::Linux_X86_64,
};

// initializeTarget — register LLVM backend components for a given family.
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

// Codegen constructor
Codegen::Codegen(const TargetConfig& config) : config_(config) {
  // Always initialize the AArch64 backend — TransISA cross-compiles to ARM64
  // regardless of the host, so we need it available even on x86-64 Linux.
  LLVMInitializeAArch64TargetInfo();
  LLVMInitializeAArch64Target();
  LLVMInitializeAArch64TargetMC();
  LLVMInitializeAArch64AsmParser();
  LLVMInitializeAArch64AsmPrinter();

  // Additionally initialize the target's own backend if it differs from AArch64
  // (needed for future x86 or RISC-V host-native targets).
  if (config_.backend == "X86") {
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmParser();
    LLVMInitializeX86AsmPrinter();
  }
  // RISCV: when added, initialize RISC-V backend here.
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

  // Use the target triple from the configuration rather than a hardcoded
  // string. This is what makes the backend modular: everything downstream —
  // data layout, instruction selection, ABI — follows from the triple.
  const std::string& targetTriple = config_.triple;
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