#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/NoFolder.h>

int main() {
    llvm::LLVMContext context;
    llvm::Module module("simple_module", context);
    llvm::IRBuilder<llvm::NoFolder> builder(context);

    llvm::FunctionType *funcType = llvm::FunctionType::get(
        builder.getInt32Ty(), // возвращаемый тип
        false                // без аргументов
    );

    llvm::Function *mainFunc = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        "main",
        module
    );

    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(
        context,
        "entry",
        mainFunc
    );
    builder.SetInsertPoint(entryBlock);

    llvm::Value *lhs = builder.getInt32(353);
    llvm::Value *rhs = builder.getInt32(48);
    llvm::Value *sum = builder.CreateAdd(lhs, rhs, "sum");

    builder.CreateRet(sum);

    module.dump();

    return 0;
}
