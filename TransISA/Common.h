#ifndef TRANSISA_COMMON_H
#define TRANSISA_COMMON_H

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <memory>

namespace TransISA {

// unique pointer to an LLVM Module, managed within a context.
using ModuleUPtr = std::unique_ptr<llvm::Module>;

} // namespace TransISA

#endif // TRANSISA_COMMON_H