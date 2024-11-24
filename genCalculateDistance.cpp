#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <map>

std::map<std::string, llvm::StructType *> struct_types;
std::map<std::string, llvm::GlobalVariable *> global_values;
std::map<std::string, llvm::Function *> functions;

llvm::StructType *addStructType(std::shared_ptr<llvm::Module> ir_module, std::string name, std::vector<llvm::Type *> fields)
{
    llvm::StructType *struct_type = llvm::StructType::create(ir_module->getContext(), name);
    struct_type->setBody(fields);
    struct_types.insert(std::make_pair(name, struct_type));
    return struct_type;
}

llvm::GlobalVariable *addGlobalValue(std::shared_ptr<llvm::Module> ir_module, std::string name, llvm::Type *type, llvm::Constant *initializer, int align)
{
    llvm::GlobalVariable *global = (llvm::GlobalVariable *)ir_module->getOrInsertGlobal(name, type);
    global->setInitializer(initializer);
    global->setDSOLocal(true);
    global->setAlignment(llvm::MaybeAlign(align));
    global_values.insert(std::make_pair(name, global));
    return global;
}

llvm::GlobalVariable *addGlobalString(std::shared_ptr<llvm::Module> ir_module, std::string name, std::string value)
{
    llvm::GlobalVariable *global = (llvm::GlobalVariable *)ir_module->getOrInsertGlobal(name, llvm::ArrayType::get(llvm::Type::getInt8Ty(ir_module->getContext()), value.size() + 1));
    global->setInitializer(llvm::ConstantDataArray::getString(ir_module->getContext(), value, true));
    global->setDSOLocal(true);
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    global->setLinkage(llvm::GlobalValue::LinkageTypes::PrivateLinkage);
    global->setConstant(true);
    global->setAlignment(llvm::MaybeAlign(1));
    global_values.insert(std::make_pair(name, global));
    return global;
}

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
    llvm::IRBuilder<> ir_builder(ir_module->getContext());

    // add struct type
    llvm::Type *Edge_s_fields[] = {llvm::Type::getInt32Ty(ir_module->getContext()), llvm::Type::getInt32Ty(ir_module->getContext()), llvm::Type::getInt64Ty(ir_module->getContext())};
    llvm::StructType *Edge_s = addStructType(ir_module, "struct.Edge_s", std::vector<llvm::Type *>(Edge_s_fields, Edge_s_fields + 3));

    // add global value
    llvm::PointerType::get(Edge_s, 0);
    llvm::GlobalVariable *edge1 = addGlobalValue(ir_module, "edge1", Edge_s, llvm::ConstantStruct::get(Edge_s, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 5)), 8);
    llvm::GlobalVariable *edge2 = addGlobalValue(ir_module, "edge2", Edge_s, llvm::ConstantStruct::get(Edge_s, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 10)), 8);
    addGlobalValue(ir_module, "allDist", llvm::ArrayType::get(llvm::ArrayType::get(llvm::Type::getInt32Ty(ir_module->getContext()), 3), 3), llvm::ConstantAggregateZero::get(llvm::ArrayType::get(llvm::ArrayType::get(llvm::Type::getInt32Ty(ir_module->getContext()), 3), 3)), 16);
    addGlobalValue(ir_module, "dist", llvm::ArrayType::get(llvm::PointerType::get(Edge_s, 0), 3), llvm::ConstantArray::get(llvm::ArrayType::get(llvm::PointerType::get(Edge_s, 0), 3), {llvm::ConstantExpr::getBitCast(edge1, llvm::PointerType::get(Edge_s, 0)), llvm::ConstantExpr::getBitCast(edge2, llvm::PointerType::get(Edge_s, 0)), llvm::ConstantPointerNull::get(llvm::PointerType::get(Edge_s, 0))}), 16);
    addGlobalValue(ir_module, "minDistance", llvm::Type::getInt64Ty(ir_module->getContext()), llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 5), 8);

    // add global string
    llvm::GlobalVariable *str = addGlobalString(ir_module, ".str", "%lld\00");
    llvm::GlobalVariable *str1 = addGlobalString(ir_module, ".str1", "%lld %lld %d\n\00");

    // add external function
    addFunction(ir_module, "__isoc99_scanf", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), llvm::PointerType::get(llvm::Type::getInt8Ty(ir_module->getContext()), 0), true));
    addFunction(ir_module, "printf", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), llvm::PointerType::get(llvm::Type::getInt8Ty(ir_module->getContext()), 0), true));
}

void buildCaculateDistance(std::shared_ptr<llvm::Module> ir_module)
{
    llvm::IRBuilder<> builder(ir_module->getContext());

    llvm::Function *caculateDistance = addFunction(ir_module, "caculateDistance", llvm::FunctionType::get(llvm::Type::getVoidTy(ir_module->getContext()), false));
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
    builder.SetInsertPoint(entry);

    // // TODO
    auto int32_ty = llvm::Type::getInt32Ty(builder.getContext());
    auto int64_ty = llvm::Type::getInt64Ty(builder.getContext());

    // NodeId k
    // LongDistance kDist
    auto var_k = builder.CreateAlloca(int32_ty, nullptr, "k");
    auto var_kDist = builder.CreateAlloca(int64_ty, nullptr, "kDist");

    // k = 0
    builder.CreateStore(llvm::ConstantInt::get(int32_ty, 0), var_k);

    llvm::BasicBlock *loop_test = llvm::BasicBlock::Create(ir_module->getContext(), "loop_test", caculateDistance);
    llvm::BasicBlock *loop_out = llvm::BasicBlock::Create(ir_module->getContext(), "loop_out", caculateDistance);
    llvm::BasicBlock *loop_body = llvm::BasicBlock::Create(ir_module->getContext(), "loop_body", caculateDistance);
    llvm::BasicBlock *kDist_lt_minDistance_true = llvm::BasicBlock::Create(ir_module->getContext(), "lt_true", caculateDistance);
    llvm::BasicBlock *kDist_lt_minDistance_false = llvm::BasicBlock::Create(ir_module->getContext(), "lt_false", caculateDistance);
    llvm::BasicBlock *kDist_lt_minDistance_out = llvm::BasicBlock::Create(ir_module->getContext(), "lt_out", caculateDistance);

    builder.CreateBr(loop_test);

    builder.SetInsertPoint(loop_test);

    // loop_test //

    // k < 3 ?
    auto loop_condition = builder.CreateICmpSLT(
        builder.CreateLoad(int32_ty, var_k),
        llvm::ConstantInt::get(int32_ty, 3));

    // jump to  loop_body / loop_out
    builder.CreateCondBr(loop_condition, loop_body, loop_out);

    // loop_test //

    builder.SetInsertPoint(loop_body);

    // loop_body //
    auto dist = global_values["dist"];

    // k
    auto var_k_val = builder.CreateLoad(int32_ty, var_k);

    // &dist[k]
    auto edge_p_addr = builder.CreateGEP(llvm::ArrayType::get(llvm::PointerType::get(struct_types["struct.Edge_s"],0),3),
                                        dist,
                                        {llvm::ConstantInt::get(int32_ty,0),var_k_val});
                            
    // disk[k]
    auto edge_p = builder.CreateLoad(llvm::PointerType::get(struct_types["struct.Edge_s"], 0), edge_p_addr);

    // &(disk[k] -> w)
    auto w_p = builder.CreateGEP(struct_types["struct.Edge_s"], edge_p, {llvm::ConstantInt::get(int32_ty, 0), llvm::ConstantInt::get(int32_ty, 2)});

    // disk[k]->w
    auto w_val = builder.CreateLoad(int64_ty, w_p);

    auto minDis_condition = builder.CreateICmpSLT(builder.CreateLoad(int64_ty, var_kDist),
                                                  builder.CreateLoad(int64_ty, global_values["minDistance"]));

    builder.CreateCondBr(minDis_condition, kDist_lt_minDistance_true, kDist_lt_minDistance_false);

    // kDist < minDistance
    builder.SetInsertPoint(kDist_lt_minDistance_true);

    auto true_branch_val = builder.CreateLoad(int64_ty, var_kDist);

    builder.CreateBr(kDist_lt_minDistance_out);

    // kDist < minDistance
    
    // kDist >= minDistance
    builder.SetInsertPoint(kDist_lt_minDistance_false);

    auto false_branch_val = builder.CreateLoad(int64_ty, global_values["minDistance"]);

    builder.CreateBr(kDist_lt_minDistance_out);

    // kDist >= minDistance

    // lt_out
    builder.SetInsertPoint(kDist_lt_minDistance_out);

    llvm::PHINode *phi = builder.CreatePHI(int64_ty, 2);
    phi->addIncoming(true_branch_val,kDist_lt_minDistance_true);
    phi->addIncoming(false_branch_val, kDist_lt_minDistance_false);

    builder.CreateStore(phi, global_values["minDistance"]);

    builder.CreateStore(
        builder.CreateNSWAdd(builder.CreateLoad(int32_ty,var_k), llvm::ConstantInt::get(int32_ty, 1)),
        var_k);

    builder.CreateBr(loop_test);

    // loop_body

    builder.SetInsertPoint(loop_out);

    builder.CreateRetVoid();
}

void buildMain(std::shared_ptr<llvm::Module> ir_module)
{
    llvm::IRBuilder<> builder(ir_module->getContext());

    llvm::Function *main = addFunction(ir_module, "main", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), false));
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ir_module->getContext(), "", main);
    builder.SetInsertPoint(entry);
    
    // TODO
    auto int32_ty = llvm::Type::getInt32Ty(builder.getContext());
    auto int64_ty = llvm::Type::getInt64Ty(builder.getContext());

    auto edge_addr = builder.CreateAlloca(struct_types["struct.Edge_s"], nullptr);

    auto edge_w_addr = builder.CreateGEP(struct_types["struct.Edge_s"], edge_addr, {llvm::ConstantInt::get(int32_ty, 0), llvm::ConstantInt::get(int32_ty, 2)});

    auto str_addr = builder.CreateGEP(global_values[".str"]->getType()->getPointerElementType(),
                                      global_values[".str"],
                                      {llvm::ConstantInt::get(int32_ty, 0),
                                      llvm::ConstantInt::get(int32_ty, 0)});

    builder.CreateCall(functions["__isoc99_scanf"], {str_addr, edge_w_addr});

    auto dist_ele_2_addr = builder.CreateGEP(global_values["dist"]->getType()->getPointerElementType(), global_values["dist"], {llvm::ConstantInt::get(int32_ty, 0), llvm::ConstantInt::get(int32_ty, 2)});

    // dist[ NUM -1 ] = &edge;
    builder.CreateStore(edge_addr, dist_ele_2_addr);

    // & allDist[0][0]
    auto allDist_0_0 = builder.CreateGEP(global_values["allDist"]->getType()->getPointerElementType(), global_values["allDist"], {llvm::ConstantInt::get(int32_ty, 0), llvm::ConstantInt::get(int32_ty, 0), llvm::ConstantInt::get(int32_ty, 0)});

    auto edge_w_value = builder.CreateLoad(int64_ty, edge_w_addr);

    auto edge_w_value_trunc = builder.CreateTrunc(edge_w_value, int32_ty);

    builder.CreateStore(edge_w_value_trunc, allDist_0_0);

    builder.CreateCall(functions["caculateDistance"]);

    auto param = builder.CreateAdd(builder.CreateLoad(int64_ty,edge_w_addr), llvm::ConstantInt::get(int64_ty, 5));
    param = builder.CreateAdd(param, llvm::ConstantInt::get(int64_ty, 10));

    auto allDist_0_0_value = builder.CreateLoad(int32_ty, allDist_0_0);

    auto str_1_addr = builder.CreateGEP(global_values[".str1"]->getType()->getPointerElementType(), global_values[".str1"], {llvm::ConstantInt::get(int32_ty, 0), llvm::ConstantInt::get(int32_ty, 0)});

    auto minDistance_val = builder.CreateLoad(global_values["minDistance"]->getType()->getPointerElementType(), global_values["minDistance"]);

    builder.CreateCall(functions["printf"], {str_1_addr, minDistance_val, param, allDist_0_0_value});

    builder.CreateRet(llvm::ConstantInt::get(int32_ty, 0));
}

void buildFunction(std::shared_ptr<llvm::Module> ir_module)
{
    buildCaculateDistance(ir_module);
    buildMain(ir_module);
}

int main(int, char **)
{
    llvm::LLVMContext context;
    std::shared_ptr<llvm::Module> ir_module = std::make_shared<llvm::Module>("calculateDistance", context);
    ir_module->setTargetTriple("x86_64-pc-linux-gnu");

    buildGlobal(ir_module);
    buildFunction(ir_module);

    ir_module->print(llvm::outs(), nullptr);

    return 0;
}
