#ifndef TRANSISA_CODEGEN_EMITTER_H
#define TRANSISA_CODEGEN_EMITTER_H

#include <string>
#include "TransISA/Common.h"

namespace TransISA::Codegen {

class Emitter {
 public:
  Emitter(const std::string& Triple);

  /// Emits the given LLVM module to an assembly file.
  bool emit(llvm::Module& M, const std::string& OutputFilename);

 private:
  const std::string& Triple;
};

} // namespace TransISA::Codegen

#endif // TRANSISA_CODEGEN_EMITTER_H