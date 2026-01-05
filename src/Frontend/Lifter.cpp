#include "TransISA/Frontend/Lifter.h"
#include "TransISA/Utils/Logger.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/TargetParser/Triple.h"

using namespace llvm;

namespace TransISA::Frontend {

// internal MC components structure
struct Lifter::MCImpl {
  const Target* TheTarget = nullptr;
  std::unique_ptr<MCRegisterInfo> MRI;
  std::unique_ptr<MCAsmInfo> MAI;
  std::unique_ptr<MCSubtargetInfo> MSTI;
  std::unique_ptr<MCInstrInfo> MII;
  std::unique_ptr<MCDisassembler> Disassembler;
  std::unique_ptr<MCContext> MCCtx;
};

Lifter::Lifter(llvm::LLVMContext& CtxRef, const std::string& TripleStr)
    : Ctx(CtxRef), Triple(TripleStr), MC(std::make_unique<MCImpl>()) {
  // targets for disassembly
  LLVMInitializeX86TargetInfo();
  LLVMInitializeX86Target();
  LLVMInitializeX86TargetMC();
  LLVMInitializeX86Disassembler();

  LLVMInitializeAArch64TargetInfo();
  LLVMInitializeAArch64Target();
  LLVMInitializeAArch64TargetMC();
  LLVMInitializeAArch64Disassembler();

  std::string Error;
  MC->TheTarget = TargetRegistry::lookupTarget(TripleStr, Error);
  if (!MC->TheTarget) {
    // TODO: Use logging here
    return;
  }

  // all the MC components
  MC->MRI.reset(MC->TheTarget->createMCRegInfo(TripleStr));
  MC->MAI.reset(
      MC->TheTarget->createMCAsmInfo(*MC->MRI, TripleStr, MCTargetOptions()));
  MC->MSTI.reset(MC->TheTarget->createMCSubtargetInfo(TripleStr, "", ""));
  MC->MII.reset(MC->TheTarget->createMCInstrInfo());

  // MCContext
  llvm::Triple TheTriple(TripleStr);
  MC->MCCtx = std::make_unique<MCContext>(
      TheTriple, MC->MAI.get(), MC->MRI.get(), MC->MSTI.get());

  // the disassembler
  MC->Disassembler.reset(
      MC->TheTarget->createMCDisassembler(*MC->MSTI, *MC->MCCtx));
}

Lifter::~Lifter() = default;

ModuleUPtr Lifter::lift(
    const std::vector<uint8_t>& MachineCode,
    const std::string& ModuleName) {
  if (!MC || !MC->Disassembler) {
    // TODO: Use logging here
    return nullptr;
  }

  auto Module = std::make_unique<llvm::Module>(ModuleName, Ctx);
  Module->setTargetTriple(Triple);

  // TODO: Implement the lifting logic here.
  // 1. Creating a Function within the Module.
  // 2. Creating BasicBlocks for the function.
  // 3. Using the MCDisassembler to decode instructions from MachineCode.
  // 4. For each MCInst, create the corresponding LLVM IR instructions.
  // 5. This is a complex process that requires mapping MCInst opcodes and
  // operands
  //    to LLVM IR.
  FunctionType* FuncType = FunctionType::get(Type::getVoidTy(Ctx), false);
  Function* Func = Function::Create(
      FuncType, Function::ExternalLinkage, ModuleName, Module.get());
  BasicBlock* BB = BasicBlock::Create(Ctx, "entry", Func);
  IRBuilder<> Builder(BB);
  Builder.CreateRetVoid();

  return Module;
}

} // namespace TransISA::Frontend