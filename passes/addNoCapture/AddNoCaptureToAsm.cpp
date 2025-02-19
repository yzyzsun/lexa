#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/IR/InlineAsm.h"
#include <string>
#include <regex>

using namespace llvm;

// AddNoCaptureToAsmPass: adds 'nocapture' annotation to specified arguments to inline assembly
class AddNoCaptureToAsmPass : public PassInfoMixin<AddNoCaptureToAsmPass>
{
public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM)
    {
        bool modified = false;

        for (BasicBlock &BB : F)
        {
            for (Instruction &I : BB)
            {
                if (CallInst *CI = dyn_cast<CallInst>(&I))
                {
                    if (CI->isInlineAsm())
                    {
                        InlineAsm *IA = cast<InlineAsm>(CI->getCalledOperand());
                        if (IA && IA->getAsmString().find("annotation_marker") != std::string::npos)
                        {
                            std::regex re("nocapture_([0-9_]+)");
                            std::smatch match;
                            if (std::regex_search(IA->getAsmString(), match, re))
                            {
                                std::string args = match[1].str();

                                std::vector<int> noCaptureArgs;
                                size_t pos = 0;
                                while ((pos = args.find("_")) != std::string::npos)
                                {
                                    noCaptureArgs.push_back(std::stoi(args.substr(0, pos)));
                                    args.erase(0, pos + 1);
                                }
                                noCaptureArgs.push_back(std::stoi(args));

                                // Mark the specified arguments as nocapture
                                for (int argIdx : noCaptureArgs)
                                {
                                    if (argIdx < CI->arg_size())
                                    {
                                        Value *Arg = CI->getArgOperand(argIdx);
                                        if (Arg->getType()->isPointerTy())
                                        {
                                            CI->addParamAttr(argIdx, Attribute::NoCapture);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return modified ? PreservedAnalyses::none() : PreservedAnalyses::all();
    }
};

// register the pass to LLVM
extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo()
{
    return {
        LLVM_PLUGIN_API_VERSION,
        "AddNoCaptureToAsm",
        "v0.1",
        [](PassBuilder &PB)
        {
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel level) {
                    FunctionPassManager FPM;
                    FPM.addPass(AddNoCaptureToAsmPass());

                    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
                }
            );

            PB.registerPipelineParsingCallback(
                [](llvm::StringRef Name, llvm::ModulePassManager &MPM, llvm::ArrayRef<llvm::PassBuilder::PipelineElement>) {
                    if (Name == "asm-addnocapture")
                    {
                        FunctionPassManager FPM;
                        FPM.addPass(AddNoCaptureToAsmPass());
                        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
                        return true;
                    }
                    return false;
                }
            );
        }
    };
}
