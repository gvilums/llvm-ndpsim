#ifndef LLVM_TRANSFORMS_INSTRUMENTATION_NDPSIM_H
#define LLVM_TRANSFORMS_INSTRUMENTATION_NDPSIM_H

#include "llvm/IR/PassManager.h"

namespace llvm {


class NDPSimPass : public PassInfoMixin<NDPSimPass> {
public:
//   explicit NDPSimPass();
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
  static bool isRequired() { return true; }
};

} // namespace llvm


#endif