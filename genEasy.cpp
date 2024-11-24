#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <map>

std::map<std::string, llvm::Function *> functions;

llvm::Function *addFunction(std::shared_ptr<llvm::Module> ir_module, std::string name, llvm::FunctionType *type)
{
    llvm::Function *function = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, ir_module.get());
    for (auto &arg : function->args())
        arg.addAttr(llvm::Attribute::NoUndef);
    functions.insert(std::make_pair(name, function));
    return function;
}

void buildGlobal(std::shared_ptr<llvm::Module> ir_module)
{
}

void buildMain(std::shared_ptr<llvm::Module> ir_module)
{
    llvm::IRBuilder<> builder(ir_module->getContext());

    llvm::Function *main = addFunction(ir_module, "main", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), false));
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ir_module->getContext(), "", main);
    builder.SetInsertPoint(entry);

    // TODO
    auto int32_ty = llvm::Type::getInt32Ty(ir_module->getContext());

    auto var_a = builder.CreateAlloca(int32_ty, nullptr, "a");
    auto var_b = builder.CreateAlloca(int32_ty, nullptr, "b");

    builder.CreateStore(llvm::ConstantInt::get(int32_ty, 1),var_a);
    builder.CreateStore(llvm::ConstantInt::get(int32_ty, 2),var_b);

    auto value_a = builder.CreateLoad(int32_ty, var_a);
    auto value_b = builder.CreateLoad(int32_ty, var_b);

    auto condition = builder.CreateICmpSLT(value_a, value_b);

    auto branch_true_a_LT_b = llvm::BasicBlock::Create(ir_module->getContext(), "branch_true_a_LT_b", main);
    auto branch_false_a_LT_b = llvm::BasicBlock::Create(ir_module->getContext(), "branch_false_a_LT_b", main);

    builder.CreateCondBr(condition, branch_true_a_LT_b, branch_false_a_LT_b);

    builder.SetInsertPoint(branch_true_a_LT_b);

    builder.CreateStore(llvm::ConstantInt::get(int32_ty, 3), var_b);
    builder.CreateBr(branch_false_a_LT_b);

    builder.SetInsertPoint(branch_false_a_LT_b);

    value_a = builder.CreateLoad(int32_ty, var_a);
    value_b = builder.CreateLoad(int32_ty, var_b);

    auto a_b_add_res = builder.CreateAdd(value_a, value_b);

    builder.CreateRet(a_b_add_res);
}

void buildFunction(std::shared_ptr<llvm::Module> ir_module)
{
    buildMain(ir_module);
}

int main()
{
    llvm::LLVMContext context;
    std::shared_ptr<llvm::Module> ir_module = std::make_shared<llvm::Module>("easy", context);
    ir_module->setTargetTriple("x86_64-pc-linux-gnu");

    buildGlobal(ir_module);
    buildFunction(ir_module);

    ir_module->print(llvm::outs(), nullptr);

    return 0;
}