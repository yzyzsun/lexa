#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/InstrTypes.h"
#include <string>
#include <regex>
#include <fstream>
#include <unistd.h>

using namespace llvm;

// AttachMetadataPass: attaches metadata to function calls as LLVM metadata,
//                     and loads tag-to-metadata mapping into the binary.
class AttachMetadataPass : public PassInfoMixin<AttachMetadataPass>
{
public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM)
    {
        bool modified = false;
        SmallPtrSet<Instruction *, 32> Markers;
        std::vector<Constant *> MappingEntries;
        LLVMContext &Ctx = M.getContext();

        for (Function &F : M)
        {
            for (BasicBlock &BB : F)
            {
                for (Instruction &I : BB)
                {
                    if (CallInst *CI = dyn_cast<CallInst>(&I))
                    {
                        Function *callee = CI->getCalledFunction();
                        if (!callee || !callee->hasName())
                            continue;

                        if (callee->getName() == "attach_metadata")
                        {
                            Value *arg = CI->getArgOperand(0);
                            // Strip pointer casts
                            if (auto *ce = dyn_cast<ConstantExpr>(arg))
                            {
                                if (ce->getOpcode() == Instruction::GetElementPtr)
                                {
                                    arg = ce->getOperand(0);
                                }
                            }

                            // Find global variable holding the string
                            if (auto *gv = dyn_cast<GlobalVariable>(arg))
                            {
                                if (auto *ca = dyn_cast<ConstantDataArray>(gv->getInitializer()))
                                {
                                    if (ca->isString())
                                    {
                                        std::string metadata = ca->getAsCString().str();

                                        // Find the function call right before attach_metadata
                                        for (auto revIt = BasicBlock::reverse_iterator(CI); revIt != BB.rend(); ++revIt)
                                        {
                                            if (auto *lastCall = dyn_cast<CallInst>(&*revIt))
                                            {
                                                Function *callee = lastCall->getCalledFunction();

                                                // skip over attach_metadata itself and any llvm intrinsic
                                                if (callee && (callee->getName() == "attach_metadata" || callee->getName().starts_with("llvm")))
                                                {
                                                    continue;
                                                }

                                                // Disable inlining for the callee
                                                // This may prevent some possible optimizations.
                                                if (Function *calledFunc = lastCall->getCalledFunction())
                                                {
                                                    if (!calledFunc->isDeclaration() && metadata[0] != '1')
                                                    {
                                                        calledFunc->addFnAttr(Attribute::NoInline);
                                                    }
                                                }
                                                
                                                // Attach "callsite.metadata" attribute to the call
                                                lastCall->addFnAttr(Attribute::get(Ctx, "callsite.metadata", metadata));
                                                modified = true;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }

                            Markers.insert(CI);
                        }
                    }
                }
            }
        }

        for (Instruction *I : Markers)
        {
            I->eraseFromParent();
        }

        return modified ? PreservedAnalyses::none() : PreservedAnalyses::all();
    }
};

class CreateLabelPass : public PassInfoMixin<CreateLabelPass>
{
    int Counter = 0;

public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM)
    {
        for (BasicBlock &BB : F)
        {
            for (Instruction &I : BB)
            {
                if (CallInst *CI = dyn_cast<CallInst>(&I))
                {
                    if (CI->hasFnAttr("callsite.metadata")) {
                        auto Attr = CI->getFnAttr("callsite.metadata");
                        StringRef metadata = Attr.getValueAsString();

                        std::string LabelName = "__callsite_metadata_${:uid}___" + metadata.str();

                        // Create a label right after the function call
                        IRBuilder<> Builder(CI->getNextNode());
                        Builder.CreateCall(
                            InlineAsm::get(FunctionType::get(Builder.getVoidTy(), false), LabelName + ":\n", "", true),
                            {});
                    }
                }
            }
        }

        return PreservedAnalyses::all();
    }
};

// register the pass to LLVM
extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo()
{
    return {
        LLVM_PLUGIN_API_VERSION,
        "AttachMetadata",
        "v0.1",
        [](PassBuilder &PB)
        {
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel level)
                {
                    MPM.addPass(AttachMetadataPass());
                });

            PB.registerOptimizerLastEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel level)
                {
                    FunctionPassManager FPM;
                    FPM.addPass(CreateLabelPass());
                    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
                });
        }};
}
