#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;

// MarkInvariantPass: Replaces the 'mark_defs_invariant' functions with llvm.invariant.start intrinsic
class MarkInvariantPass : public PassInfoMixin<MarkInvariantPass>
{
public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM)
    {
        bool modified = false;
        SmallPtrSet<Instruction*, 32> Markers;
        LLVMContext &Ctx = F.getContext();
        Module *M = F.getParent();

        FunctionCallee InvariantStart = M->getOrInsertFunction(
            "llvm.invariant.start.p0",
            FunctionType::get(PointerType::get(Ctx, 0), {Type::getInt64Ty(Ctx), PointerType::get(Ctx, 0)}, false));

        for (auto &BB : F)
        {
            for (auto &I : BB)
            {
                if(CallInst *CI = dyn_cast<CallInst>(&I)) {
                    if(CI->getCalledFunction() && CI->getCalledFunction()->getName() == "mark_defs_invariant") {
                        IRBuilder<> Builder(&I);

                        Value *stub = CI->getOperand(0);
                        size_t num_defs = dyn_cast<ConstantInt>(CI->getOperand(1))->getZExtValue();
                        modified = true;

                        Value *GEPI;
                        
                        for(size_t i = 0; i < num_defs; ++i) {
                            GEPI = Builder.CreateInBoundsGEP(Type::getInt64Ty(Ctx), stub, Builder.getInt64(i * 2));
                            Builder.CreateCall(InvariantStart, {ConstantInt::get(Type::getInt64Ty(Ctx), 8), GEPI});

                            GEPI = Builder.CreateInBoundsGEP(Type::getInt64Ty(Ctx), stub, Builder.getInt64(i * 2 + 1));
                            Builder.CreateCall(InvariantStart, {ConstantInt::get(Type::getInt64Ty(Ctx), 8), GEPI});
                        }

                        Markers.insert(CI);
                    }
                }
            }
        }

        for(Instruction *I: Markers) {
            I->eraseFromParent();
        }

        return modified ? PreservedAnalyses::none() : PreservedAnalyses::all();
    }
};

// register the pass to LLVM
extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo()
{
    return {
        LLVM_PLUGIN_API_VERSION,
        "MarkInvariant",
        "v0.1",
        [](PassBuilder &PB)
        {
            PB.registerPipelineEarlySimplificationEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel level) {
                    FunctionPassManager FPM;
                    FPM.addPass(MarkInvariantPass());
                    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
                }
            );


            PB.registerPipelineParsingCallback(
                [](llvm::StringRef Name, llvm::ModulePassManager &MPM, llvm::ArrayRef<llvm::PassBuilder::PipelineElement>){
                    if (Name == "markinv") {
                        FunctionPassManager FPM;
                        FPM.addPass(MarkInvariantPass());
                        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
                        return true;
                    }
                    return false;
                }
            );
        }
    };
}
