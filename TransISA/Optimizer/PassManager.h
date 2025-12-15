#ifndef TRANSISA_OPTIMIZER_PASSMANAGER_H
#define TRANSISA_OPTIMIZER_PASSMANAGER_H

#include "TransISA/Common.h"

namespace TransISA::Optimizer {

class PassManager {
 public:
  PassManager() = default;

  /// runs a set of optimization passes on the given module.
  void optimize(llvm::Module& M);
};

} // namespace TransISA::Optimizer

#endif // TRANSISA_OPTIMIZER_PASSMANAGER_H
