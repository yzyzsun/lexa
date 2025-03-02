#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;

class GenerateStackMapPass : public PassInfoMixin<GenerateStackMapPass>
{
public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM)
    {
        SmallPtrSet<Instruction*, 32> Markers;
        LLVMContext &Ctx = F.getContext();
        Module *M = F.getParent();

        IRBuilder<> Builder(Ctx);
        Builder.SetInsertPointPastAllocas(&F);

        for (auto &BB : F)
        {
            for (auto &I : BB)
            {
                if(CallInst *CI = dyn_cast<CallInst>(&I)) {
                    if(CI->getCalledFunction() && CI->getCalledFunction()->getName() == "stackmap") {
                        IRBuilder<> Builder(&I);
                        Value *stub = CI->getOperand(0);

                        std::vector<Value*> Args;
                        Args.push_back(ConstantInt::get(Type::getInt64Ty(F.getContext()), stringToInt(trim(F.getName().str(), "_"))));
                        Args.push_back(ConstantInt::get(Type::getInt32Ty(F.getContext()), 0));
                        Args.push_back(stub);
                        Function *StackMapFn = Intrinsic::getDeclaration(M, Intrinsic::experimental_stackmap);
                        auto stackMapI = Builder.CreateCall(StackMapFn, Args);
                        stackMapI->addParamAttr(2, Attribute::ReadOnly);
                        stackMapI->addParamAttr(2, Attribute::NoCapture);

                        Markers.insert(CI);
                    }
                }

            }
        }

        for(Instruction *I: Markers) {
            I->eraseFromParent();
        }

        return PreservedAnalyses::all();
    }

private:

    unsigned long stringToInt(const std::string& str) {
        unsigned long result = 0;
        for (char c : str) {
            result = result * 256 + static_cast<unsigned char>(c);
        }
        return result;
    }

    std::string trim(const std::string& str, const std::string& chars = " \t\n\r\f\v") {
        size_t start = str.find_first_not_of(chars);
        if (start == std::string::npos)
            return ""; // No content other than trim characters

        size_t end = str.find_last_not_of(chars);
        return str.substr(start, end - start + 1);
    }
};

// register the pass to LLVM
extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo()
{
    return {
        LLVM_PLUGIN_API_VERSION,
        "GenerateStackMapPass",
        "v0.1",
        [](PassBuilder &PB)
        {
            PB.registerOptimizerLastEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel level) {
                    FunctionPassManager FPM;
                    FPM.addPass(GenerateStackMapPass());
                    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
                }
            );
        }
    };
}
