#include "llvm/Transforms/Instrumentation/NDPSim.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"

using namespace llvm;

PreservedAnalyses NDPSimPass::run(Function &F, FunctionAnalysisManager &AM) {
  LLVMContext &Ctx = F.getContext();
  Module *Parent = F.getParent();
  if (Parent == nullptr) {
    // Cannot define functions when there is no module, so skip
    return PreservedAnalyses::all();
  }
  auto *Int64Type = Type::getInt64Ty(Ctx);
  auto *Int8PtrType = Type::getInt8PtrTy(Ctx);
  FunctionCallee InstrTraceFunc = Parent->getOrInsertFunction(
      "_ndp_sim_dynamic_instr", Type::getVoidTy(Ctx), Int64Type);

  FunctionCallee LoadTraceFunc = Parent->getOrInsertFunction(
      "_ndp_sim_memload", Type::getVoidTy(Ctx), Int8PtrType);

  FunctionCallee StoreTraceFunc = Parent->getOrInsertFunction(
      "_ndp_sim_memstore", Type::getVoidTy(Ctx), Int8PtrType);

    auto Attrs = F.getAttributes();
    // skip functions which should not be simulated
    if (!Attrs.hasFnAttr(Attribute::NDPSimulate)) {
      return PreservedAnalyses::all();
    }
    for (auto &B : F) {
      const size_t BlockSize = B.size();

      // First, insert dynamic instruction count tracing
      if (auto *Location = B.getTerminator()) {
        auto *InstrCountConst = ConstantInt::get(Int64Type, BlockSize);

        IRBuilder<> Builder(Location);
        Builder.CreateCall(InstrTraceFunc, {InstrCountConst});
      }

      // Then, insert tracing for loads and stores
      for (auto &I : B) {
        if (auto *LI = dyn_cast<LoadInst>(&I)) {
          auto *Arg = LI->getPointerOperand();

          IRBuilder<> Builder(LI);
          auto *CastArg = Builder.CreateBitCast(Arg, Int8PtrType);
          Builder.CreateCall(LoadTraceFunc, {CastArg});
        }
        if (auto *SI = dyn_cast<StoreInst>(&I)) {
          auto *Arg = SI->getPointerOperand();

          IRBuilder<> Builder(SI);
          auto *CastArg = Builder.CreateBitCast(Arg, Int8PtrType);
          Builder.CreateCall(StoreTraceFunc, {CastArg});
        }
      }

    }
  // TODO check which analyses are actually preserved
  //
  // For example, we don't modify the CFG, so that should be preserved
  return PreservedAnalyses::none();
}