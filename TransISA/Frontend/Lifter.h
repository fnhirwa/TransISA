#ifndef TRANSISA_FRONTEND_LIFTER_H
#define TRANSISA_FRONTEND_LIFTER_H

#include <string>
#include <vector>
#include "TransISA/Common.h"

namespace TransISA::Frontend {

/**
 * @brief Lifts machine code from one ISA to LLVM IR
 *
 * The Lifter class provides functionality to convert binary machine code
 * into LLVM IR, which can then be optimized and recompiled for a different
 * target architecture.
 */
class Lifter {
 public:
  /**
   * @brief Constructs a new Lifter object
   *
   * @param Ctx The LLVM context to use for creating IR
   * @param Triple The target triple string (e.g., "x86_64-unknown-linux-gnu")
   */
  Lifter(llvm::LLVMContext& Ctx, const std::string& Triple);
  /**
   * @brief Destroys the Lifter object
   */
  ~Lifter();

  /**
   * @brief Lifts a buffer of machine code into an LLVM module
   *
   * @param MachineCode A vector containing the raw machine code bytes
   * @param ModuleName The name to give to the generated LLVM module
   * @return ModuleUPtr A unique pointer to the generated LLVM module, or
   * nullptr on failure
   */
  ModuleUPtr lift(
      const std::vector<uint8_t>& MachineCode,
      const std::string& ModuleName);

 private:
  llvm::LLVMContext& Ctx;
  const std::string& Triple;

  // LLVM MC layer components will be stored here
  struct MCImpl;
  std::unique_ptr<MCImpl> MC;
};

} // namespace TransISA::Frontend

#endif // TRANSISA_FRONTEND_LIFTER_H