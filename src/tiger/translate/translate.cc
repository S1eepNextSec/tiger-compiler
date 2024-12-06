#include "tiger/translate/translate.h"

#include <tiger/absyn/absyn.h>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/x64frame.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <stack>
#include <unordered_set>
#include <unordered_map>

extern frame::Frags *frags;
extern frame::RegManager *reg_manager;
extern llvm::IRBuilder<> *ir_builder;
extern llvm::Module *ir_module;
std::stack<llvm::Function *> func_stack;  
std::stack<llvm::BasicBlock *> loop_stack;  // 记录循环嵌套的跳出位置
llvm::Function *alloc_record;
llvm::Function *init_array;
llvm::Function *string_equal;
std::vector<std::pair<std::string, frame::Frame *>> frame_info;

bool CheckBBTerminatorIsBranch(llvm::BasicBlock *bb) {
  auto inst = bb->getTerminator();
  if (inst) {
    llvm::BranchInst *branchInst = llvm::dyn_cast<llvm::BranchInst>(inst);
    if (branchInst && !branchInst->isConditional()) {
      return true;
    }
  }
  return false;
}

int getActualFramesize(tr::Level *level) {
  return level->frame_->calculateActualFramesize();
}
/**
 * HINT
 * -  通过AST来生成IR的时候，每当遇到一个变量
 *    就需要知道他是在哪个FUNCTION对应的FRAME里面声明出来的
 *    通过对应的ACCESS结构来根据偏移量加减栈指针以获取变量
 *    因此需要有一个结构来追溯所有变量的LEVEL & ACCESS
 *    ProgTr内部有 Tenv & Venv结构 每个Tenv中的entry会记录
 *    Ty & Ty对应在llvm里面的llvm::Type
 * 
 * -  每个Type::Ty的子类都实现了GetLLVMType的函数 
 *    以此来获取AST中变量类型Type::Ty对应的llvm::Type
 * 
 * -  Venv中记录的 VEntry 中附带了tr::Access字段
 *    tr::Access包含 Level信息 & frame::Access信息
 *    -  Level可以帮助获取到变量所在函数结构中的层级
 *    -  frame::Access可以帮助获取其在对应栈帧中的位置
 *    因此在构造Venv的时候需要传入对应的 tr::Access结构
 * 
 * -  Venv中记录的 FunEntry 中附带了 Level* 字段
 *    Level结构中包含指向parent Level指针以及对应的frame指针
 *      - 通过 frame 可以获取到函数对应的栈帧结构
 *      - 通过 level 可以获取函数在嵌套层级中的父函数
 *    因此在FunEntry中添加函数
 *      - 先构造 Frame
 *      - 根据 Frame 构造出 Level
 *      - 将 Level & llvm function 传入 FunEntry 构造函数
 * 
 * -  IR中 每次进入一个函数 会根据传入的栈顶指针 
 *    减去当前函数对应的栈帧大小 来获取函数栈帧顶部位置
 *    因此当所有函数的栈帧分析完毕时 需要添加栈帧的全局变量
 * 
 * -  每个函数中的本地变量进行 Var Dec 的时候
 *    需要知道是在哪个函数的 scope 下进行声明的
 *    所以需要知道当前函数对应的 Frame 因此将 Level 层层向下传递
 *    根据 level 可以获取到 current function scope
 * 
 * -  Translate过程中 根据当前层的Level获取到函数的stack pointer
 *    是合法的 因为此时必定在该函数对应的 llvm ir function内
 *    对应的 stack pointer 在该llvm ir function内是有效的值
 *    但是跨越 Level 访问别的函数的 stack pointer 不合法
 *    因为 llvm ir function 中不能访问别的函数定义中的值
 *    别的函数的 stack pointer 是在他自己的域内计算出来的 不能在别处使用
 *    
 * 
 */

/**
 * -  Prob_12.2:
 *    FunctionDec中进入具体函数进行分析时
 *    没有在 venv 中添加形参的VEntry
 *    需要重写 FunctionDec 中的逻辑 <------ 已解决
 */

/**
 * - Prob_12.3:
 *   - 需要调整类型检查 保证出现Nil的上下文中必定可以推断出Nil所属的Record 类型 <----- 已解决
 *   - 需要调整类型检查 保证RecordExp中Field赋值为Nil 其Field类型必定得为Record <----- 已解决
 * 
 */

/**
 * - Prob_12.6:
 *   去除使用 select IR 的部分  <----- 已解决
 *   布尔相关的运算向上传递 i1  <----- 已解决
 * 
 * - Prob_12.6:
 *   - 需要调整类型检查 避免不同类型的比较 / 逻辑运算出现 <-----  已解决
 *   - 设计exp的地方需要检查如果是 i32 是不是需要将 可能返回的 i1 转换为 i32  <----- 已解决
 */
inline auto getLLVMConstantInt1(int value) -> llvm::Constant *
{
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(ir_module->getContext()), value);
}

inline auto getLLVMConstantInt32(int value) -> llvm::Constant *
{
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), value);
}

inline auto getLLVMConstantInt64(long long value) -> llvm::Constant *
{
    return llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), value);
}

inline auto getLLVMTypeInt32() -> llvm::Type *
{
    return llvm::Type::getInt32Ty(ir_module->getContext());
}

inline auto getLLVMTypeInt64() -> llvm::Type *
{
    return llvm::Type::getInt64Ty(ir_module->getContext());
}

inline auto getLLVMTypeInt32Ptr() -> llvm::PointerType *
{
    return llvm::Type::getInt32PtrTy(ir_module->getContext());
}

inline auto getLLVMTypeInt64Ptr() -> llvm::PointerType *
{
    return llvm::Type::getInt64PtrTy(ir_module->getContext());
}


namespace tr {

inline void generateRuntimeFunction(){
    auto alloc_record_type_llvm = llvm::FunctionType::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                                                          {llvm::Type::getInt32Ty(ir_module->getContext())},
                                                          false);
    alloc_record = llvm::Function::Create(alloc_record_type_llvm,
                                          llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                                          "alloc_record",
                                          *ir_module);

    auto init_array_type_llvm = llvm::FunctionType::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                                                        {llvm::Type::getInt32Ty(ir_module->getContext()),
                                                         llvm::Type::getInt64Ty(ir_module->getContext())},
                                                        false);

    init_array = llvm::Function::Create(init_array_type_llvm,
                                        llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                                        "init_array",
                                        *ir_module);

    // auto string_equal_type_llvm = llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()),
    //                                                       {type::StringTy::Instance()->GetLLVMType(),
    //                                                        type::StringTy::Instance()->GetLLVMType()},
    //                                                       false);
    auto string_equal_type_llvm = llvm::FunctionType::get(llvm::Type::getInt1Ty(ir_module->getContext()),
                                                          {type::StringTy::Instance()->GetLLVMType(),
                                                           type::StringTy::Instance()->GetLLVMType()},
                                                          false);

    string_equal = llvm::Function::Create(string_equal_type_llvm,
                                          llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                                          "string_equal",
                                          *ir_module);
}


inline llvm::Value *generateGetGlobalFramesizeIR(frame::Frame *frame)
{
    auto global_framesize_value_llvm = ir_builder->CreateLoad(frame->framesize_global->getType()->getPointerElementType(),
                                                              frame->framesize_global,
                                                              frame->name_->Name() + "_frame_size");

    return global_framesize_value_llvm;
}

/**
 * @param 要设置本地变量的函数 Frame 对应的 Level 结构
 * @param 本地变量在 Frame 中相对于 Frame Top 的偏移量
 * @return 指向本地变量的LLVM Value* (PointerType)
 */
llvm::Value* generateSetLocalEscapeInt32IR(tr::Access * tr_access,llvm::Value* value_to_set){
    const auto & level = tr_access->level_;
    const auto & offset = tr_access->access_->get_offset();

    auto global_framesize_value_llvm = tr::generateGetGlobalFramesizeIR(tr_access->level_->frame_);

    // calculate local variable offset
    auto local_var_offset_llvm = ir_builder->CreateAdd(global_framesize_value_llvm,
                                                  llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                                                                         offset));

    // get frame stack pointer
    auto frame_stack_pointer = level->get_sp();

    auto local_var_addr_in_int64 = ir_builder->CreateAdd(frame_stack_pointer, local_var_offset_llvm);

    // 原本地址值在llvm ir中被识别为 int64
    // 需要转为 int32* 才能执行load指令
    auto local_var_addr_llvm = ir_builder->CreateIntToPtr(local_var_addr_in_int64,
                                                     llvm::Type::getInt32PtrTy(ir_module->getContext(), 0));

    ir_builder->CreateStore(value_to_set,
                            local_var_addr_llvm);

    return local_var_addr_llvm;
}

/**
 * @return 返回的llvm::Value*中Value实际是 int32* 的抽象 即是变量的地址
 * 因为不知道使用到该变量是作为左值还是右值
 * 如果是左值就需要对地址进行 Store
 * 如果是右值就需要对地址进行 Load
 */
llvm::Value* generateAccessLocalEscapeInt32IR(tr::Access * tr_access){
    auto global_framesize_val_llvm = tr::generateGetGlobalFramesizeIR(tr_access->level_->frame_);

    auto offset = tr_access->access_->get_offset();

    auto local_var_offset = ir_builder->CreateAdd(global_framesize_val_llvm,
                                                  llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                                                                         offset));

    auto frame_sp = tr_access->level_->get_sp();

    auto local_var_addr_in_int64 = ir_builder->CreateAdd(frame_sp,
                                                         local_var_offset);

    auto local_var_addr_llvm = ir_builder->CreateIntToPtr(local_var_addr_in_int64,
                                                          llvm::Type::getInt32PtrTy(ir_module->getContext(),
                                                                                    0));

    return local_var_addr_llvm;
}

Access *Access::AllocLocal(Level *level, bool escape) {
  return new Access(level, level->frame_->AllocLocal(escape));
}

class ValAndTy {
public:
  type::Ty *ty_;
  llvm::Value *val_;
  llvm::BasicBlock *last_bb_;

  ValAndTy(llvm::Value *val, type::Ty *ty) : val_(val), ty_(ty), last_bb_(ir_builder->GetInsertBlock()) {}
  ValAndTy(llvm::Value *val, type::Ty *ty, llvm::BasicBlock* last_bb) : val_(val), ty_(ty), last_bb_(last_bb){}
};

void ProgTr::OutputIR(std::string_view filename) {
  std::string llvmfile = std::string(filename) + ".ll";
  std::error_code ec;
  llvm::raw_fd_ostream out(llvmfile, ec, llvm::sys::fs::OpenFlags::OF_Text);
  ir_module->print(out, nullptr);
}

void ProgTr::Translate() {
  FillBaseVEnv();
  FillBaseTEnv();
  /* TODO: Put your lab5-part1 code here */
  tr::generateRuntimeFunction();

  tenv_->BeginScope();
  venv_->BeginScope();

  /**
   * ProgTr 初始化的时候已经构造好了一个tigermain的栈帧 以及 对应的 Level
   */
  auto tigermain_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()),
                                                {llvm::Type::getInt64Ty(ir_module->getContext()),
                                                 llvm::Type::getInt64Ty(ir_module->getContext())},
                                                false);

  auto tigermain_fun = llvm::Function::Create(tigermain_type,
                                              llvm::Function::ExternalLinkage,
                                              this->main_level_->frame_->name_->Name(),
                                              *ir_module);

  auto tigermain_entry = new env::FunEntry(this->main_level_.get(),
                                           new type::TyList(),
                                           type::IntTy::Instance(),
                                           tigermain_type,
                                           tigermain_fun);

  this->venv_->Enter(this->main_level_->frame_->name_,
                     tigermain_entry);

  // 因为 tenv 中会有scope变化
  // scope退出的时候函数就无法通过venv来访问
  // 需要记录过程中所有的函数 最终设置 global frame size
  frame_info.push_back({this->main_level_->frame_->name_->Name(), this->main_level_->frame_});

  auto tigermain_bb = llvm::BasicBlock::Create(ir_module->getContext(), "tigermain", tigermain_fun);

  ir_builder->SetInsertPoint(tigermain_bb);

  // 获取 tigermain的栈帧开始位置
  auto tigermain_framesize_global = ir_module->getGlobalVariable("tigermain_framesize_global");
  auto frame_size_value = ir_builder->CreateLoad(tigermain_framesize_global->getType()->getPointerElementType(),
                         tigermain_framesize_global);

  llvm::Value* last_frame_sp = tigermain_fun->arg_begin();

  auto tigermain_sp = ir_builder->CreateSub(last_frame_sp, frame_size_value,"tigermain_sp");

  // 设置tigermain的栈指针
  this->main_level_->set_sp(tigermain_sp);

  this->absyn_tree_->Translate(this->venv_.get(),
                               this->tenv_.get(),
                               this->main_level_.get(),
                               this->errormsg_.get());
  
  // 所有分析完之后 调整所有函数的 global frame size
  for (const auto & name_function_pair : frame_info){
      auto global_frame_size_val = name_function_pair.second->calculateActualFramesize();

      auto global_frame_size = ir_module->getGlobalVariable(name_function_pair.first + "_framesize_global");

      global_frame_size->setInitializer(llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()),
                                                            global_frame_size_val));

      global_frame_size->setConstant(true);
  }

  this->tenv_->EndScope();
  this->venv_->EndScope();

  ir_builder->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0));
}

} // namespace tr

namespace absyn {

tr::ValAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */

  // frame::Frame

  this->root_->Translate(venv, tenv, level, errormsg);

  return nullptr;
}

void TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv, tr::Level *level,
                        err::ErrorMsg *errormsg) const {
 /* TODO: Put your lab5-part1 code here */

 std::list<type::NameTy *> indirect_types;  // 非直接的Type
 std::unordered_set<absyn::NameAndTy *> skip_types; // 重复声明

 // type ID = array of ID / ID / { ID : ID, ID : ID ...}
 // 先将 type dec 里的 header 加入 tenv
 for (const auto &absyn_name_ty_ptr : this->types_->GetList()) {
     auto entry = tenv->Look(absyn_name_ty_ptr->name_);

     if (entry != nullptr) {
         errormsg->Error(this->pos_, "two types have the same name");
         skip_types.insert(absyn_name_ty_ptr);
         continue;
     }

     auto ty = new type::NameTy(absyn_name_ty_ptr->name_, nullptr);

     tenv->Enter(absyn_name_ty_ptr->name_, ty);
 }

 for (const auto &absyn_name_ty_ptr : this->types_->GetList()) {
     if (skip_types.find(absyn_name_ty_ptr) != skip_types.end()) {
         // do not analyze duplicated type dec
         continue;
     }

     auto l_type = tenv->Look(absyn_name_ty_ptr->name_);
     
     
     
     auto l_type_derived = static_cast<type::NameTy *>(l_type);

     // what will happen if r_type gets a nullptr?
     // in type::NameTy & type::ArrayTy & type::RecordTy ensures that
     // when analyze a absyn::Ty won't get a nullptr
     auto r_type = absyn_name_ty_ptr->ty_->Translate(tenv, errormsg);

     // NameTy Case
     if (typeid(*r_type) == typeid(type::NameTy)){
         auto r_name_ty_derived = static_cast<type::NameTy *>(r_type);

         indirect_types.push_back(l_type_derived);
     }

     l_type_derived->ty_ = r_type;
 }

 // TODO:cycle ?

 // cycle check
 // cycle will be detected but not cut down
 // is there any problem?
 std::unordered_set<type::Ty *> set;
 bool isCycleFound = false;
 for (auto type_to_check : indirect_types) {
     auto origin = type_to_check;

     set.clear();
     set.insert(type_to_check);

     while (typeid(*type_to_check->ty_) == typeid(type::NameTy)){
         type_to_check = static_cast<type::NameTy *>(type_to_check->ty_);

         if (set.find(type_to_check)!= set.end()){
             errormsg->Error(this->pos_, "illegal type cycle");
             isCycleFound = true;
             break;
         }

         set.insert(type_to_check);
     }

     if (isCycleFound) {
         break;
     }
 }
}

void FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *level, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */

  // 先简单地语义分析出所有函数的 参数列表 & 返回类型

  // venv 中加入FunctionDec中的所有声明
  std::unordered_map<sym::Symbol *, env::FunEntry *> fun_name_entry_map;

  // add header to venv
  for (const auto &absyn_fun_dec : this->functions_->GetList()) {
      // 构造 ty list
      auto ty_list = absyn_fun_dec->params_->MakeFormalTyList(tenv, errormsg);

      // 构造 escape list
      std::list<bool> escape_list;

      for (const auto & field : absyn_fun_dec->params_->GetList()){
          escape_list.push_back(field->escape_);
      }

      // 构造 result ty
      type::Ty *result_ty = type::VoidTy::Instance();

      if (absyn_fun_dec->result_ != nullptr) {
          result_ty = tenv->Look(absyn_fun_dec->result_)->ActualTy();
      }

      // last_sp & static link
      std::vector<llvm::Type *> param_type_llvm_list = {getLLVMTypeInt64(),
                                                        getLLVMTypeInt64()};

      for (const auto & param_ty : ty_list->GetList()){
          param_type_llvm_list.push_back(param_ty->GetLLVMType());
      }

      llvm::ArrayRef<llvm::Type *> params_llvm_array_ref(param_type_llvm_list);

      // 构造 func_type
      auto func_type_llvm = llvm::FunctionType::get(result_ty->GetLLVMType(),
                                                    params_llvm_array_ref,
                                                    false);

      // 构造 function
      auto func_llvm = llvm::Function::Create(func_type_llvm,
                                              llvm::GlobalValue::ExternalLinkage,
                                              absyn_fun_dec->name_->Name(),
                                              *ir_module);

      // 构造 fun_entry
      auto fun_entry = new env::FunEntry(new tr::Level(frame::NewFrame(absyn_fun_dec->name_,
                                                                       escape_list),
                                                       level),
                                         ty_list,
                                         result_ty,
                                         func_type_llvm,
                                         func_llvm);

      venv->Enter(absyn_fun_dec->name_, fun_entry);

      fun_name_entry_map.insert({absyn_fun_dec->name_, fun_entry});
      
      frame_info.push_back({absyn_fun_dec->name_->Name(), fun_entry->level_->frame_});
  }

  for (const auto & absyn_fun_dec : this->functions_->GetList()){
      auto last_bb = ir_builder->GetInsertBlock();

      venv->BeginScope();
      /**
       * 理论上来说这里应该根据变量的逃逸状况来设置
       * 对于每个逃逸的变量才将形参复制到栈上
       * 对于 static link 是必定将值复制到栈上的
       */

      /**
       * -  生成 Basic Block 调整 InsertPoint
       * -  设置 frame stack pointer
       * -  复制 static link 至 栈上
       * -  复制 形参 至 栈上
       */
      const auto &fun_name_symbol = absyn_fun_dec->name_;
      const auto &fun_llvm = fun_name_entry_map[fun_name_symbol]->func_;
      const auto &fun_level = fun_name_entry_map[fun_name_symbol]->level_;
      auto fun_basic_block = llvm::BasicBlock::Create(ir_module->getContext(),
                                                      fun_name_symbol->Name(),
                                                      fun_llvm);
      
      
      ir_builder->SetInsertPoint(fun_basic_block);

      const auto & frame_size = tr::generateGetGlobalFramesizeIR(fun_level->frame_);


      /* 设置 frame stack pointer */
      auto arg_llvm = fun_llvm->args().begin();

      auto last_sp_llvm = (llvm::Value*)arg_llvm;

      auto current_sp_llvm = ir_builder->CreateSub(last_sp_llvm, frame_size);

      fun_level->set_sp(current_sp_llvm);

      /* 复制 static link */
      arg_llvm++;

      auto var_access_it = fun_level->frame_->formals_->begin();

      auto sl_offset = (*var_access_it)->get_offset();

      auto static_link_addr_llvm_int64 = ir_builder->CreateAdd(last_sp_llvm,
                                                               getLLVMConstantInt64(sl_offset));

      auto static_link_addr_llvm = ir_builder->CreateIntToPtr(static_link_addr_llvm_int64,
                                                              getLLVMTypeInt64Ptr());

      ir_builder->CreateStore((llvm::Value* )arg_llvm,
                              static_link_addr_llvm);

      /* 复制 形参 */
      arg_llvm++;
      var_access_it++;

      auto ty = fun_name_entry_map[fun_name_symbol]->formals_->GetList().begin();
      auto formal_name_it = absyn_fun_dec->params_->GetList().begin();

      while (ty != fun_name_entry_map[fun_name_symbol]->formals_->GetList().end()){
        auto offset = (*var_access_it)->get_offset();

        auto var_name_symbol = (*formal_name_it)->name_;

        auto var_addr_llvm_int64 = ir_builder->CreateAdd(last_sp_llvm,
                                                         getLLVMConstantInt64(offset));

        auto var_addr_llvm = ir_builder->CreateIntToPtr(var_addr_llvm_int64,
                                                        llvm::PointerType::get((*ty)->GetLLVMType(),0));

        venv->Enter(var_name_symbol,
                    new env::VarEntry(new tr::Access(fun_level,
                                                     (*var_access_it)),
                                      (*ty)->ActualTy(),
                                      false));

        ir_builder->CreateStore((llvm::Value*)arg_llvm,
                                var_addr_llvm);

        arg_llvm++;
        var_access_it++;
        ty++;
        formal_name_it++;
      }

      // 开始翻译函数体
      auto ret_val_ty = absyn_fun_dec->body_->Translate(venv, tenv, fun_level, errormsg);

      auto ret_val_llvm = ret_val_ty->val_;
      const auto & ret_ty = ret_val_ty->ty_;

      if (ret_ty->ActualTy() == type::IntTy::Instance() && ret_val_llvm->getType()->isIntegerTy(1)){
          ret_val_llvm = ir_builder->CreateZExt(ret_val_llvm,
                                                getLLVMTypeInt32());
      }

      if (ret_ty->IsSameType(type::VoidTy::Instance())){
          // ret void
          ir_builder->CreateRetVoid();
      } else {
          ir_builder->CreateRet(ret_val_llvm);
      }

      venv->EndScope();

      ir_builder->SetInsertPoint(last_bb);
  }
}

void VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv, tr::Level *level,
                       err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */

  // VAR ID := exp / VAR ID : type := exp

  auto exp_val_ty = this->init_->Translate(venv, tenv,level,errormsg);

  const auto & exp_value_llvm = exp_val_ty->val_;
  const auto & exp_ty = exp_val_ty->ty_;

  // type check start
  if (this->typ_ != nullptr) {
      auto var_type = tenv->Look(this->typ_);

      // var_type may be nullptr
      if (var_type == nullptr) {
          errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
      }

      if (var_type && !var_type->IsSameType(exp_ty)) {
          errormsg->Error(this->pos_, "type mismatch");
      }
  }

  if (this->typ_ == nullptr && exp_ty->ActualTy() == type::NilTy::Instance()) {
      errormsg->Error(this->pos_, "init should not be nil without type specified");
  }

  // type check finished

  /**
   * 生成对应 IR 指令
   * -  获取栈帧大小 global variable
   * -  根据 offset 调整得出local variable所在位置的偏移量
   * -  当前栈指针加上位置偏移量得到 local variable 的实际地址
   * -  地址转为指针类型
   * -  存储值
   */

  // 在对应的函数 Frame 中分配空间
  // 构造Level & Access
  // 创建 VarEntry
  const auto &current_function_frame = level->frame_;

  // 直接在栈上分配空间而非寄存器
  // 保证最终栈帧大小计算正确
  auto frame_access = current_function_frame->AllocLocal(true);

  auto tr_access = new tr::Access(level, frame_access);

  auto var_entry = new env::VarEntry(tr_access, exp_ty->ActualTy(), false);

  venv->Enter(this->var_, var_entry);

  auto framesize = tr::generateGetGlobalFramesizeIR(current_function_frame);

  const auto &offset = tr_access->access_->get_offset();

  auto local_var_offset = ir_builder->CreateAdd(framesize,
                                                getLLVMConstantInt64(offset));

  auto frame_sp = level->get_sp();

  auto var_addr_llvm_int64 = ir_builder->CreateAdd(frame_sp, local_var_offset);

  auto var_addr_llvm = ir_builder->CreateIntToPtr(var_addr_llvm_int64,
                                                  llvm::PointerType::get(exp_ty->GetLLVMType(), 0));

  ir_builder->CreateStore(exp_value_llvm, var_addr_llvm);
}

type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  auto type = tenv->Look(this->name_);

  if (type == nullptr) {
      errormsg->Error(this->pos_, "undefined type %s", this->name_->Name().data());

      // undefined type , default void
      type = type::VoidTy::Instance();
  }
  // transfer

  // type won't be a nullptr
  // it's pretty safe
  return type;
}

type::Ty *RecordTy::Translate(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;

  // transfer
  type::FieldList *type_field_list = new type::FieldList();

  for (const auto &absyn_field : this->record_->GetList()) {
      const auto &field_name = absyn_field->name_;
      const auto &type_name = absyn_field->typ_;

      auto type = tenv->Look(type_name);

      if (type == nullptr) {
          errormsg->Error(this->pos_, "undefined type %s", type_name->Name().data());

          // undefined type , default set it to void
          type = type::VoidTy::Instance();
      }

      type_field_list->Append(new type::Field(field_name, type));
  }

  // now the record type will not contain any nullptr
  // it's pretty safe
  return new type::RecordTy(type_field_list);
}

type::Ty *ArrayTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;

  // get element type in tenv
  auto element_type = tenv->Look(this->array_);

  if (element_type == nullptr) {
      errormsg->Error(this->pos_, "undefined type %s", this->array_->Name().data());

      // default set the element_type to void type
      element_type = type::VoidTy::Instance();
  }

  return new type::ArrayTy(element_type);
}

/**
 * 若 Simple Var 对应变量LLVM中类型为 T
 * 返回 llvm::Value*[T*]
 * -  type 为 int -> llvm::Value*[int32*]
 * -  type 为 record -> llvm::Value*[struct **] (record 实际类型为 struct *)
 * -  type 为 array -> llvm::Value*[element_type **] (element_type为array中元素的类型,array实际类型为element_type*)
 * 返回的是变量的地址 因为不确定是左值 / 右值
 */
tr::ValAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  auto entry = venv->Look(this->sym_);

  auto var_entry = static_cast<env::VarEntry *>(entry);

  /**
   * 访问变量
   * 只能访问当前函数所在的函数定义嵌套层级中的变量
   * -  tr::Level* 指向相同 -> 说明就是在当前函数
   * -  tr::Level* 指向不同 -> 说明在祖先函数中
   */

  auto cur_level = level;
  auto cur_frame_sp = level->get_sp();

  while (cur_level != var_entry->access_->level_){
      /* 根据 static link 一级一级向上查找对应的 frame Instance */
      auto frame_size = tr::generateGetGlobalFramesizeIR(cur_level->frame_);

      auto last_sp = ir_builder->CreateAdd(cur_frame_sp, frame_size);
  
      auto sl_addr_llvm_int64 = ir_builder->CreateAdd(last_sp,
                                                      getLLVMConstantInt64(8));

      auto sl_addr_llvm = ir_builder->CreateIntToPtr(sl_addr_llvm_int64,
                                                     getLLVMTypeInt64Ptr());

      auto cur_sl = ir_builder->CreateLoad(getLLVMTypeInt64(),
                                           sl_addr_llvm);

      cur_frame_sp = cur_sl;

      cur_level = cur_level->parent_;
  }

  const auto &offset = var_entry->access_->access_->get_offset();
  const auto &ty = var_entry->ty_;

  auto frame_size = tr::generateGetGlobalFramesizeIR(cur_level->frame_);

  auto var_offset = ir_builder->CreateAdd(frame_size,
                                          getLLVMConstantInt64(offset),
                                          this->sym_->Name() + "_offset");

  auto var_addr_llvm_int64 = ir_builder->CreateAdd(cur_frame_sp,
                                                   var_offset,
                                                   this->sym_->Name() + "_addr_int64");

  // return value ? or addr of value ?
  auto var_addr_llvm = ir_builder->CreateIntToPtr(var_addr_llvm_int64,
                                                  llvm::PointerType::get(ty->GetLLVMType(),0),
                                                  this->sym_->Name()+"_pointer");

  // better return addr of value !

  return new tr::ValAndTy(var_addr_llvm, ty->ActualTy());

}

/**
 * 若 Record 中 Field对应的LLVM类型为T
 * 返回 llvm::Value*[T*]
 * -  type 为 record -> llvm::Value*[struct **] (record 实际类型为 struct *)
 * -  type 为 array -> llvm::Value*[element_type **] (element_type为array中元素的类型,array实际类型为element_type*)
 * 返回的是变量的地址 因为不确定是左值 / 右值
 */
tr::ValAndTy *FieldVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;

  auto var_val_ty = this->var_->Translate(venv, tenv, level, errormsg);

  // struct **
  const auto &var_addr_llvm = var_val_ty->val_;
  
  // ty: struct
  const auto &var_ty = var_val_ty->ty_;

  // FieldVar 
  // var 必定是一个 recordTy 对应的 Var

  auto record_ty = static_cast<type::RecordTy *>(var_ty->ActualTy());

  int element_index = 0;
  type::Ty *element_ty = nullptr;

  // FIXME: 应该在 type checking 的时候反映出有可能域名错误?
  for (const auto & field : record_ty->fields_->GetList()){
    if (field->name_ == this->sym_){
        element_ty = field->ty_->ActualTy();
        break;
    }

    element_index++;
  }

  auto record_p = ir_builder->CreateLoad(var_addr_llvm->getType()->getPointerElementType(),
                                         var_addr_llvm);

  // record_p : struct *
  // element_p : element *
  auto element_p = ir_builder->CreateGEP(record_p->getType()->getPointerElementType(),
                                         record_p,
                                         {getLLVMConstantInt32(0),
                                          getLLVMConstantInt32(element_index)});

  // 得到对应域的地址
  // 返回地址

  return new tr::ValAndTy(element_p, element_ty->ActualTy());
}

tr::ValAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level,
                                      err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  auto array_val_ty = this->var_->Translate(venv, tenv, level, errormsg);

  auto ty = array_val_ty->ty_;

  auto array_ty = static_cast<type::ArrayTy *>(ty->ActualTy());

  auto element_ty = array_ty->ty_->ActualTy();

  auto index_val_ty = this->subscript_->Translate(venv, tenv, level, errormsg);

  auto index = index_val_ty->val_;

  if (index->getType()->isIntegerTy(1)){
      index = ir_builder->CreateZExt(index, getLLVMTypeInt32());
  }

  auto array_element_p_addr_llvm = array_val_ty->val_;

  auto array_element_p_llvm = ir_builder->CreateLoad(array_element_p_addr_llvm->getType()->getPointerElementType(),
                                                     array_element_p_addr_llvm);

  auto element_addr = ir_builder->CreateGEP(array_element_p_llvm->getType()->getPointerElementType(),
                                            array_element_p_llvm,
                                            {index});

  return new tr::ValAndTy(element_addr, element_ty->ActualTy());
}

tr::ValAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  auto var_val_ty = this->var_->Translate(venv, tenv, level, errormsg);

  // 返回的llvm值是变量的地址
  // 作为表达式 应该是作为右值

  auto rvalue_llvm = ir_builder->CreateLoad(var_val_ty->val_->getType()->getPointerElementType(),
                         var_val_ty->val_,
                         var_val_ty->val_->getName() + "_to_rvalue");

  return new tr::ValAndTy(rvalue_llvm, var_val_ty->ty_);
}

tr::ValAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  return new tr::ValAndTy(nullptr, type::NilTy::Instance());
}

tr::ValAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;

  /**
   * 生成LLVM常量返回
   */
  auto int_val = llvm::ConstantInt::get(getLLVMTypeInt32(),
                                        this->val_);

  return new tr::ValAndTy(int_val, type::IntTy::Instance());
}

tr::ValAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  auto str_struct_ptr = type::StringTy::CreateGlobalStringStructPtr(this->str_);

  return new tr::ValAndTy(str_struct_ptr, type::StringTy::Instance());
}

tr::ValAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  
  auto v_entry = venv->Look(this->func_);

  auto fun_entry = static_cast<env::FunEntry *>(v_entry);

  // 调整当前函数的 outgo space
  level->frame_->AllocOutgoSpace(fun_entry->level_->frame_->formals_->size() * 8);

  auto current_sp = level->get_sp();

  auto current_level = level;

  auto callee_level = fun_entry->level_;

  // no parent !
  // main_level !
  // external function call
  if (callee_level->parent_ == nullptr){
      std::vector<llvm::Value *> llvm_call_params = {};

      for (const auto &formal : this->args_->GetList()) {
          auto formal_val_ty = formal->Translate(venv, tenv, level, errormsg);

          llvm_call_params.push_back(formal_val_ty->val_);
      }

      llvm::ArrayRef llvm_call_params_array_ref(llvm_call_params);

      auto call_ret_val_llvm = ir_builder->CreateCall(fun_entry->func_,
                                                      llvm_call_params_array_ref);

      return new tr::ValAndTy(call_ret_val_llvm, fun_entry->result_->ActualTy());
  }

  auto sl_search_sp = level->get_sp();

  while (current_level != callee_level->parent_){
      auto frame_size = tr::generateGetGlobalFramesizeIR(current_level->frame_);

      auto sl_offset = ir_builder->CreateAdd(frame_size,
                                             getLLVMConstantInt64(8));

      auto sl_addr_llvm_int64 = ir_builder->CreateAdd(sl_search_sp, sl_offset);

      auto sl_addr_llvm = ir_builder->CreateIntToPtr(sl_addr_llvm_int64,
                                                     getLLVMTypeInt64Ptr());

      sl_search_sp = ir_builder->CreateLoad(sl_addr_llvm->getType()->getPointerElementType(),
                                            sl_addr_llvm);

      current_level = current_level->parent_;
  }

  // sl_search_sp 为 llvm::Value*[int64] int64值的意义是static link 是父函数的栈指针
  std::vector<llvm::Value *> llvm_call_params = {current_sp,sl_search_sp};

  for (const auto & formal : this->args_->GetList()){
    auto formal_val_ty = formal->Translate(venv,tenv,level,errormsg);

    llvm_call_params.push_back(formal_val_ty->val_);
  }

  llvm::ArrayRef llvm_call_params_array_ref(llvm_call_params);
  
  auto call_ret_val_llvm = ir_builder->CreateCall(fun_entry->func_,
                                                  llvm_call_params_array_ref);

  return new tr::ValAndTy(call_ret_val_llvm, fun_entry->result_->ActualTy());

  // return nullptr;
}

/**
 * OpExp 原本设计是比较运算以及逻辑运算全部都通过 select 返回 int32的结果
 *       改为比较运算以及逻辑运算都返回 i1 的结果
 *       如果碰到需要将比较运算的结果参与到i32的运算
 */
tr::ValAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  if (this->oper_ == Oper::OR_OP){
      auto right_cond_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                                   "right_exp_test",
                                                   ir_builder->GetInsertBlock()->getParent());
      auto or_next_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                                   "or_next",
                                                   ir_builder->GetInsertBlock()->getParent());

      auto left_cond_bb = ir_builder->GetInsertBlock();

      right_cond_bb->moveAfter(left_cond_bb);
      or_next_bb->moveAfter(right_cond_bb);

      auto left_val_ty = this->left_->Translate(venv, tenv, level, errormsg);

      auto left_condition = left_val_ty->val_;

      auto left_bb = left_val_ty->last_bb_;

      // 保证 left_condition 是 i1
      if (left_condition->getType()->isIntegerTy(32)){
          left_condition = ir_builder->CreateICmpNE(left_condition,
                                                    getLLVMConstantInt32(0));
      } else if (left_condition->getType()->isIntegerTy(64)){
          left_condition = ir_builder->CreateICmpNE(left_condition,
                                                    getLLVMConstantInt64(0));
      } else if (left_condition->getType()->isPointerTy()){
          left_condition = ir_builder->CreatePtrToInt(left_condition,
                                                      getLLVMTypeInt64());
          left_condition = ir_builder->CreateICmpNE(left_condition,
                                                  getLLVMConstantInt64(0));
      }

      ir_builder->CreateCondBr(left_condition,
                               or_next_bb, 
                               right_cond_bb);

      ir_builder->SetInsertPoint(right_cond_bb);

      auto right_val_ty = this->right_->Translate(venv, tenv, level, errormsg);

      auto right_condition = right_val_ty->val_;

      auto right_bb = right_val_ty->last_bb_;

      // 保证 right_condition 是 i1
      if (right_condition->getType()->isIntegerTy(32)) {
          right_condition = ir_builder->CreateICmpNE(right_condition,
                                                    getLLVMConstantInt32(0));
      } else if (right_condition->getType()->isIntegerTy(64)) {
          right_condition = ir_builder->CreateICmpNE(right_condition,
                                                    getLLVMConstantInt64(0));
      } else if (right_condition->getType()->isPointerTy()) {
          right_condition = ir_builder->CreatePtrToInt(right_condition,
                                                      getLLVMTypeInt64());
          right_condition = ir_builder->CreateICmpNE(right_condition,
                                                    getLLVMConstantInt64(0));
      }

      ir_builder->CreateBr(or_next_bb);

      ir_builder->SetInsertPoint(or_next_bb);

      auto phi = ir_builder->CreatePHI(left_condition->getType(), 2, "or_combine");
      phi->addIncoming(left_condition, left_bb);
      phi->addIncoming(right_condition, right_bb);

      // auto res = ir_builder->CreateSelect(phi,
      //                                     llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 1),
      //                                     llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0));

      return new tr::ValAndTy(phi, type::IntTy::Instance());

      // auto left = this->left_->Translate(venv, tenv, level, errormsg);
      // auto right = this->right_->Translate(venv, tenv, level, errormsg);

      // auto llor = ir_builder->CreateOr(left->val_, right->val_);

      // return new tr::ValAndTy(llor, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::AND_OP) {
      auto right_cond_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                                    "right_exp_test",
                                                    ir_builder->GetInsertBlock()->getParent());
      auto and_next_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                                 "and_next",
                                                 ir_builder->GetInsertBlock()->getParent());

      auto left_cond_bb = ir_builder->GetInsertBlock();

      right_cond_bb->moveAfter(left_cond_bb);
      and_next_bb->moveAfter(right_cond_bb);

      auto left_val_ty = this->left_->Translate(venv, tenv, level, errormsg);

      auto left_condition = left_val_ty->val_;

      auto left_bb = left_val_ty->last_bb_;

      // 保证 left_condition 是 i1
      if (left_condition->getType()->isIntegerTy(32)) {
          left_condition = ir_builder->CreateICmpNE(left_condition,
                                                    getLLVMConstantInt32(0));
      } else if (left_condition->getType()->isIntegerTy(64)) {
          left_condition = ir_builder->CreateICmpNE(left_condition,
                                                    getLLVMConstantInt64(0));
      } else if (left_condition->getType()->isPointerTy()) {
          left_condition = ir_builder->CreatePtrToInt(left_condition,
                                                      getLLVMTypeInt64());
          left_condition = ir_builder->CreateICmpNE(left_condition,
                                                    getLLVMConstantInt64(0));
      }

      ir_builder->CreateCondBr(left_condition,
                               right_cond_bb,
                               and_next_bb);

      ir_builder->SetInsertPoint(right_cond_bb);

      auto right_val_ty = this->right_->Translate(venv, tenv, level, errormsg);

      auto right_condition = right_val_ty->val_;

      auto right_bb = right_val_ty->last_bb_;

      // 保证 right_condition 是 i1
      if (right_condition->getType()->isIntegerTy(32)) {
          right_condition = ir_builder->CreateICmpNE(right_condition,
                                                     getLLVMConstantInt32(0));
      } else if (right_condition->getType()->isIntegerTy(64)) {
          right_condition = ir_builder->CreateICmpNE(right_condition,
                                                     getLLVMConstantInt64(0));
      } else if (right_condition->getType()->isPointerTy()) {
          right_condition = ir_builder->CreatePtrToInt(right_condition,
                                                       getLLVMTypeInt64());
          right_condition = ir_builder->CreateICmpNE(right_condition,
                                                     getLLVMConstantInt64(0));
      }

      ir_builder->CreateBr(and_next_bb);

      ir_builder->SetInsertPoint(and_next_bb);

      auto phi = ir_builder->CreatePHI(left_condition->getType(), 2, "and_combine");
      phi->addIncoming(left_condition, left_bb);
      phi->addIncoming(right_condition, right_bb);

      return new tr::ValAndTy(phi, type::IntTy::Instance());
      // auto left = this->left_->Translate(venv, tenv, level, errormsg);
      // auto right = this->right_->Translate(venv, tenv, level, errormsg);

      // auto lland = ir_builder->CreateAnd(left->val_, right->val_);

      // return new tr::ValAndTy(lland, type::IntTy::Instance());
  }

  auto left_val_ty = this->left_->Translate(venv, tenv, level, errormsg);

  auto right_val_ty = this->right_->Translate(venv, tenv, level, errormsg);

  auto left_ty = left_val_ty->ty_->ActualTy();
  auto right_ty = right_val_ty->ty_->ActualTy();
  auto left_val = left_val_ty->val_;
  auto right_val = right_val_ty->val_;

  // 基于所有 + - * / 比较 都必须在同类型之间运算
  // + - * / 只能是 i32 与 i32 tiger中没有i1
  // 比较只能是i32 与 i32 | record 与 record(NIL) | array 与 array | string 与 string 没有 i1
  if (left_ty == type::IntTy::Instance() && left_val->getType()->isIntegerTy(1)) {
      left_val = ir_builder->CreateZExt(left_val, getLLVMTypeInt32());
  }

  if (right_ty == type::IntTy::Instance() && right_val->getType()->isIntegerTy(1)) {
      right_val = ir_builder->CreateZExt(right_val, getLLVMTypeInt32());
  }

  if (this->oper_ == Oper::PLUS_OP) {
      auto res = ir_builder->CreateAdd(left_val_ty->val_, right_val_ty->val_);

      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::MINUS_OP) {
      auto res = ir_builder->CreateSub(left_val_ty->val_, right_val_ty->val_);

      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::DIVIDE_OP) {
      auto res = ir_builder->CreateSDiv(left_val_ty->val_, right_val_ty->val_);

      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::TIMES_OP) {
      auto res = ir_builder->CreateMul(left_val_ty->val_, right_val_ty->val_);

      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::EQ_OP){
    // 在类型检查的时候保证这里不会出现 Nil = Nil 的情况
    if (left_ty == type::NilTy::Instance() && typeid(*right_ty) == typeid(type::RecordTy) ||
        right_ty == type::NilTy::Instance() && typeid(*left_ty) == typeid(type::RecordTy)){
        auto addr_int64 = left_ty == type::NilTy::Instance() ? ir_builder->CreatePtrToInt(right_val, getLLVMTypeInt64()) : ir_builder->CreatePtrToInt(left_val, getLLVMTypeInt64());

        auto res = ir_builder->CreateICmpEQ(addr_int64, getLLVMConstantInt64(0));

        // auto cmp_res = ir_builder->CreateSelect(res,
        //                                         getLLVMConstantInt32(1),
        //                                         getLLVMConstantInt32(0));
        // return new tr::ValAndTy(cmp_res, type::IntTy::Instance());

        // 返回 i1
        return new tr::ValAndTy(res, type::IntTy::Instance());
    }

    if (left_ty->IsSameType(type::StringTy::Instance()) && right_ty->IsSameType(type::StringTy::Instance())){
        auto string_equal_res_int32 = ir_builder->CreateCall(string_equal,
                                                             {left_val,
                                                              right_val},
                                                             "string_equal_result");

        // 返回 i1
        return new tr::ValAndTy(string_equal_res_int32, type::IntTy::Instance());
    }

    auto res = ir_builder->CreateICmpEQ(left_val_ty->val_,right_val_ty->val_);

    // auto cmp_res = ir_builder->CreateSelect(res,
    //                                         getLLVMConstantInt32(1),
    //                                         getLLVMConstantInt32(0));

    // return new tr::ValAndTy(cmp_res, type::IntTy::Instance());

    // 返回 i1
    return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::NEQ_OP) {
      // 在类型检查的时候保证这里不会出现 Nil = Nil 的情况
      if (left_ty->ActualTy() == (type::NilTy::Instance()) && typeid(*right_ty) == typeid(type::RecordTy) ||
          right_ty->ActualTy() == (type::NilTy::Instance()) && typeid(*left_ty) == typeid(type::RecordTy)) {
          auto addr_int64 = left_ty->IsSameType(type::NilTy::Instance()) ? ir_builder->CreatePtrToInt(right_val, llvm::Type::getInt64Ty(ir_module->getContext())) : ir_builder->CreatePtrToInt(left_val, llvm::Type::getInt64Ty(ir_module->getContext()));

          auto res = ir_builder->CreateICmpNE(addr_int64, getLLVMConstantInt64(0));

          // auto cmp_res = ir_builder->CreateSelect(res,
          //                                         getLLVMConstantInt32(1),
          //                                         getLLVMConstantInt32(0));

          // return new tr::ValAndTy(cmp_res, type::IntTy::Instance());

          // 返回 i1
          return new tr::ValAndTy(res, type::IntTy::Instance());
      }

      if (left_ty->IsSameType(type::StringTy::Instance()) && right_ty->IsSameType(type::StringTy::Instance())) {
          auto string_equal_res_int1 = ir_builder->CreateCall(string_equal,
                                                               {left_val,
                                                                right_val},
                                                               "string_equal_result");

          // auto ne_res = ir_builder->CreateSub(getLLVMConstantInt32(1),
          //                                     string_equal_res_int32,
          //                                     "string_not_equal_result");

          // return new tr::ValAndTy(ne_res, type::IntTy::Instance());

          auto ne_res = ir_builder->CreateSub(getLLVMConstantInt1(1),
                                              string_equal_res_int1,
                                              "string_not_equal_result");

          return new tr::ValAndTy(ne_res, type::IntTy::Instance());
      }

      auto res = ir_builder->CreateICmpNE(left_val_ty->val_, right_val_ty->val_);

      // auto cmp_res = ir_builder->CreateSelect(res,
      //                                         getLLVMConstantInt32(1),
      //                                         getLLVMConstantInt32(0));

      // return new tr::ValAndTy(cmp_res, type::IntTy::Instance());

      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::GT_OP) {
      auto res = ir_builder->CreateICmpSGT(left_val_ty->val_, right_val_ty->val_);

      // auto cmp_res = ir_builder->CreateSelect(res,
      //                                         getLLVMConstantInt32(1),
      //                                         getLLVMConstantInt32(0));

      // return new tr::ValAndTy(cmp_res, type::IntTy::Instance());
      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::GE_OP) {
      auto res = ir_builder->CreateICmpSGE(left_val_ty->val_, right_val_ty->val_);

      // auto cmp_res = ir_builder->CreateSelect(res,
      //                                         getLLVMConstantInt32(1),
      //                                         getLLVMConstantInt32(0));

      // return new tr::ValAndTy(cmp_res, type::IntTy::Instance());
      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::LE_OP) {
      auto res = ir_builder->CreateICmpSLE(left_val_ty->val_, right_val_ty->val_);

      // auto cmp_res = ir_builder->CreateSelect(res,
      //                                         getLLVMConstantInt32(1),
      //                                         getLLVMConstantInt32(0));

      // return new tr::ValAndTy(cmp_res, type::IntTy::Instance());
      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  if (this->oper_ == Oper::LT_OP) {
      auto res = ir_builder->CreateICmpSLT(left_val_ty->val_, right_val_ty->val_);

      // auto cmp_res = ir_builder->CreateSelect(res,
      //                                         getLLVMConstantInt32(1),
      //                                         getLLVMConstantInt32(0));

      // return new tr::ValAndTy(cmp_res, type::IntTy::Instance());
      return new tr::ValAndTy(res, type::IntTy::Instance());
  }

  return nullptr;
}

/**
 * 返回 llvm::Value*[struct*]
 */
tr::ValAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  int total_size = 0;

  auto type_entry = tenv->Look(this->typ_);
  

  //假设已经经过类型检查 不会出现类型错误

  type::RecordTy *record_type = static_cast<type::RecordTy *>(type_entry->ActualTy());

  // 需要计算出 record实际占用多少字节
  // 如果是 int 是 4 字节 
  // 如果是 string 存储的是指针 8字节
  // 如果是 record 存储的是指针 8字节
  // 简单考虑 直接所有都采用 8 字节(当然会分配出多余的空间)
  total_size = 8 * record_type->fields_->GetList().size();

  // 得到的是一个 struct * 的值抽象 实际类型是 int64
  auto struct_p_llvm_int64 = ir_builder->CreateCall(alloc_record,
                                                    {getLLVMConstantInt32(total_size)});

  auto struct_p_llvm = ir_builder->CreateIntToPtr(struct_p_llvm_int64,
                                                  record_type->GetLLVMType());

  int element_index = 0;
  auto field_it = record_type->fields_->GetList().begin();

  for (const auto & efield : this->fields_->GetList()){
      auto exp_val_ty = efield->exp_->Translate(venv, tenv, level, errormsg);

      auto element_addr_p_llvm = ir_builder->CreateGEP(struct_p_llvm->getType()->getPointerElementType(),
                                                       struct_p_llvm,
                                                       {getLLVMConstantInt32(0),
                                                        getLLVMConstantInt32(element_index)});

      if (exp_val_ty->ty_->ActualTy() == type::NilTy::Instance()){
          auto null_constant = llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>((*field_it)->ty_->GetLLVMType()));

          ir_builder->CreateStore(null_constant, element_addr_p_llvm);

      } else {
        auto val_llvm = exp_val_ty->val_;

        if (val_llvm->getType()->isIntegerTy(1)){
            val_llvm = ir_builder->CreateZExt(val_llvm, getLLVMTypeInt32());
        }

        const auto &ty = exp_val_ty->ty_;

        ir_builder->CreateStore(val_llvm, element_addr_p_llvm);
      }

      
      element_index++;
      field_it++;
  }

  // 返回 struct *的值抽象 实际类型是 struct *

  return new tr::ValAndTy(struct_p_llvm, record_type);
}

tr::ValAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  tr::ValAndTy *last_val_ty = nullptr;
  for (const auto &exp : this->seq_->GetList()) {
      last_val_ty = exp->Translate(venv, tenv, level, errormsg);
  }

  return last_val_ty;
}

tr::ValAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  auto l_value_val_ty = this->var_->Translate(venv, tenv, level, errormsg);

  auto exp_val_ty = this->exp_->Translate(venv, tenv, level, errormsg);

  auto exp_val = exp_val_ty->val_;

  if (exp_val_ty->ty_->ActualTy() == type::IntTy::Instance() &&
      exp_val->getType()->isIntegerTy(1)){
      exp_val = ir_builder->CreateZExt(exp_val, getLLVMTypeInt32());
  }

  ir_builder->CreateStore(exp_val_ty->val_,
                          l_value_val_ty->val_);

  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

tr::ValAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  auto last_bb = ir_builder->GetInsertBlock();
  auto current_function_lvm = last_bb->getParent();

  auto if_test_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                             "if_test",
                                             current_function_lvm);
  auto if_then_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                             "if_then",
                                             current_function_lvm);

  auto if_next_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                             "if_next",
                                             current_function_lvm);

  llvm::BasicBlock *if_else_bb = nullptr;

  if_test_bb->moveAfter(last_bb);
  if_then_bb->moveAfter(if_test_bb);
  if_next_bb->moveAfter(if_then_bb);

  if (this->elsee_) {
      if_else_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                                 "if_else",
                                                 current_function_lvm);

      if_else_bb->moveAfter(if_then_bb);
  }

  tr::ValAndTy *then_body_ret_val_llvm = nullptr;
  tr::ValAndTy *else_body_ret_val_llvm = nullptr;

  /**
   * origin basic block
   */
  ir_builder->CreateBr(if_test_bb);

  /**
   * Transalte if test
   */
  ir_builder->SetInsertPoint(if_test_bb);

  // auto test_val_ty = this->test_->Translate(venv, tenv, level, errormsg);

  // auto test_condition = ir_builder->CreateICmpNE(test_val_ty->val_,
  //                                                getLLVMConstantInt32(0));

  auto test_condition = this->test_->Translate(venv,tenv,level,errormsg)->val_;

  // 保证 test_condition 是 i1
  if (test_condition->getType()->isIntegerTy(32)){
      test_condition = ir_builder->CreateICmpNE(test_condition,
                                                getLLVMConstantInt32(0));
  } else if (test_condition->getType()->isIntegerTy(64)){
      test_condition = ir_builder->CreateICmpNE(test_condition,
                                                getLLVMConstantInt64(0));
  } else if (test_condition->getType()->isPointerTy()){
      auto condition_int64 = ir_builder->CreatePtrToInt(test_condition, getLLVMTypeInt64());

      test_condition = ir_builder->CreateICmpNE(test_condition,
                                                getLLVMConstantInt64(0));
  }

  if (this->elsee_){
      ir_builder->CreateCondBr(test_condition,
                               if_then_bb,
                               if_else_bb);
  } else {
      ir_builder->CreateCondBr(test_condition,
                               if_then_bb,
                               if_next_bb);
  }

  /**
   * Translate if then body
   */
  ir_builder->SetInsertPoint(if_then_bb);

  then_body_ret_val_llvm =  this->then_->Translate(venv, tenv, level, errormsg);

  ir_builder->CreateBr(if_next_bb);

  /**
   * Translate if else body
   */
  if (this->elsee_){
      if_else_bb->moveAfter(if_then_bb);

      ir_builder->SetInsertPoint(if_else_bb);

      else_body_ret_val_llvm = this->elsee_->Translate(venv, tenv, level, errormsg);

      ir_builder->CreateBr(if_next_bb);
  }

  /**
   * Back Track to if next 
   */
  ir_builder->SetInsertPoint(if_next_bb);

  // auto phi_val_llvm = ir_builder->CreatePHI()
  if (this->elsee_ && ! else_body_ret_val_llvm->ty_->IsSameType(type::VoidTy::Instance())){
    // 假设前面已经做过类型检查
    // 存在类型检查上问题的程序不会进行translate
    // 这样子保证 translate 中不会存在类型上的问题

    // 如果存在 record & nil
    // 只有可能一个是 nil
    auto ret_type = then_body_ret_val_llvm->ty_->ActualTy() == type::NilTy::Instance() ? 
      else_body_ret_val_llvm->ty_->ActualTy() : then_body_ret_val_llvm->ty_->ActualTy();

    auto then_body_ret_val = then_body_ret_val_llvm->ty_->ActualTy() == type::NilTy::Instance() ? 
      llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(else_body_ret_val_llvm->ty_->GetLLVMType())) : 
      then_body_ret_val_llvm->val_;

    if (then_body_ret_val_llvm->ty_->ActualTy() == type::IntTy::Instance() && 
        then_body_ret_val_llvm->val_->getType()->isIntegerTy(1)){
        then_body_ret_val = ir_builder->CreateZExt(then_body_ret_val,
                                                   getLLVMTypeInt32());
    }

    auto else_body_ret_val = else_body_ret_val_llvm->ty_->ActualTy() ==type::NilTy::Instance() ? 
      llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(then_body_ret_val_llvm->ty_->GetLLVMType())) : 
      else_body_ret_val_llvm->val_;

    if (else_body_ret_val_llvm->ty_->ActualTy() == type::IntTy::Instance() &&
        else_body_ret_val_llvm->val_->getType()->isIntegerTy(1)) {
        else_body_ret_val = ir_builder->CreateZExt(else_body_ret_val,
                                                   getLLVMTypeInt32());
    }

    auto phi = ir_builder->CreatePHI(ret_type->GetLLVMType(),
                                     2);

    phi->addIncoming(then_body_ret_val, then_body_ret_val_llvm->last_bb_);
    phi->addIncoming(else_body_ret_val, else_body_ret_val_llvm->last_bb_);

    auto ret_val_ty = new tr::ValAndTy(phi, ret_type);
    ret_val_ty->last_bb_ = if_next_bb;

    return ret_val_ty;
  }
  
  auto ret_val_ty = new tr::ValAndTy(nullptr, type::VoidTy::Instance());
  ret_val_ty->last_bb_ = if_next_bb;

  return ret_val_ty;
}

tr::ValAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  auto last_bb = ir_builder->GetInsertBlock();
  auto current_function_llvm = last_bb->getParent();

  auto while_test_bb = llvm::BasicBlock::Create(
      ir_module->getContext(),
      "while_test",
      current_function_llvm);
  
  auto while_body_bb = llvm::BasicBlock::Create(
      ir_module->getContext(),
      "while_body",
      current_function_llvm);

  auto while_next_bb = llvm::BasicBlock::Create(
      ir_module->getContext(),
      "while_next",
      current_function_llvm);

  while_test_bb->moveAfter(last_bb);
  while_body_bb->moveAfter(while_test_bb);
  while_next_bb->moveAfter(while_body_bb);

  /* 维护 loop stack */
  loop_stack.push(while_next_bb);

  /* origin basic block */
  ir_builder->CreateBr(while_test_bb);

  /* while test */
  ir_builder->SetInsertPoint(while_test_bb);

  auto while_test_val_ty = this->test_->Translate(venv, tenv, level, errormsg);

  auto while_condition = while_test_val_ty->val_;

  if (while_condition->getType()->isIntegerTy(32)) {
      while_condition = ir_builder->CreateICmpNE(while_condition,
                                                getLLVMConstantInt32(0));
  } else if (while_condition->getType()->isIntegerTy(64)) {
      while_condition = ir_builder->CreateICmpNE(while_condition,
                                                getLLVMConstantInt64(0));
  } else if (while_condition->getType()->isPointerTy()) {
      auto condition_int64 = ir_builder->CreatePtrToInt(while_condition, getLLVMTypeInt64());

      while_condition = ir_builder->CreateICmpNE(while_condition,
                                                getLLVMConstantInt64(0));
  }

  ir_builder->CreateCondBr(while_condition, while_body_bb, while_next_bb);

  /* while body */
  ir_builder->SetInsertPoint(while_body_bb);

  // body 无返回值
  this->body_->Translate(venv, tenv, level, errormsg);

  ir_builder->CreateBr(while_test_bb);
  /* while next */
  ir_builder->SetInsertPoint(while_next_bb);

  /* 维护 loop stack */
  loop_stack.pop();

  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

tr::ValAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;

  // 先生成 lo & hi 计算相关的 LLVM IR 
  // 与原Basic Block 在同一块
  auto lo_val_ty = this->lo_->Translate(venv, tenv, level, errormsg);
  auto hi_val_ty = this->hi_->Translate(venv, tenv, level, errormsg);

  auto lo_llvm = lo_val_ty->val_;
  auto hi_llvm = hi_val_ty->val_;
  const auto & lo_ty = lo_val_ty->ty_;
  const auto & hi_ty = hi_val_ty->ty_;

  if (lo_llvm->getType()->isIntegerTy(1)){
      lo_llvm = ir_builder->CreateZExt(lo_llvm, getLLVMTypeInt32());
  }

  if (hi_llvm->getType()->isIntegerTy(1)) {
      hi_llvm = ir_builder->CreateZExt(hi_llvm, getLLVMTypeInt32());
  }

  venv->BeginScope();

  /**
   * 生成 LLVM IR
   * -  原Basic Block 中设置循环变量
   * -  从原Basic Block 跳转到 for_body
   * - for_incre
   *  - 增加循环变量
   *  - 检验条件
   *  - 继续执行则跳转至 for_body
   *  - 不执行则跳转至 for_next
   * - for_body
   *  - 执行循环体
   *  - 跳转至 for_incre
   * - for_next
   *  - 执行之后的代码块 
   */

  // 循环变量
  auto loop_var_entry = new env::VarEntry(new tr::Access(level,
                                                  level->frame_->AllocLocal(true)),
                                   type::IntTy::Instance(),
                                   true);

  venv->Enter(this->var_,
              loop_var_entry);
  
  // 生成设置循环变量的IR
  // auto i_addr = tr::generateSetLocalEscapeInt32IR(i_entry->access_,
  //                                                 lo_llvm);

  llvm::Value *loop_var_addr = nullptr;
  {
      const auto &offset = loop_var_entry->access_->access_->get_offset();
      const auto &sp = loop_var_entry->access_->level_->get_sp();
      auto global_frame_size = tr::generateGetGlobalFramesizeIR(loop_var_entry->access_->level_->frame_);

      auto loop_var_offset = ir_builder->CreateAdd(global_frame_size,
                                                   getLLVMConstantInt64(offset),
                                                   this->var_->Name()+"_offset");

      auto loop_var_addr_llvm_int64 = ir_builder->CreateAdd(sp, loop_var_offset,
                                                            this->var_->Name() + "_addr_int64");

      loop_var_addr = ir_builder->CreateIntToPtr(loop_var_addr_llvm_int64,
                                                getLLVMTypeInt32Ptr(),
                                                 this->var_->Name() + "_pointer");

      ir_builder->CreateStore(lo_llvm, loop_var_addr);
  }

  auto for_body_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                              "for_body",
                                              ir_module->getFunction(level->frame_->GetLabel()));

  auto for_next_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                              "for_next",
                                              ir_module->getFunction(level->frame_->GetLabel()));

  auto for_incre_bb = llvm::BasicBlock::Create(ir_module->getContext(),
                                              "for_incre",
                                              ir_module->getFunction(level->frame_->GetLabel()));

  // 获取原本的代码块
  auto last_basic_block = ir_builder->GetInsertBlock();

  for_incre_bb->moveAfter(last_basic_block);
  for_body_bb->moveAfter(for_incre_bb);
  for_next_bb->moveAfter(for_body_bb);

  /* 维护 loop stack */
  loop_stack.push(for_next_bb);

  // 跳转
  auto is_for_exec_condition = ir_builder->CreateICmpSLE(lo_llvm,
                                                         hi_llvm,
                                                         "for_test_condition");

  ir_builder->CreateCondBr(is_for_exec_condition, for_body_bb, for_next_bb);

  // for_incre

  ir_builder->SetInsertPoint(for_incre_bb);

  auto loop_var_llvm = ir_builder->CreateLoad(getLLVMTypeInt32(),
                                        loop_var_addr,
                                        "loop_var");

  auto new_loop_var_llvm = ir_builder->CreateAdd(loop_var_llvm,
                                 getLLVMConstantInt32(1),
                                 "loop_var_incred");

  ir_builder->CreateStore(new_loop_var_llvm, loop_var_addr);

  auto is_for_continue_condition = ir_builder->CreateICmpSLE(new_loop_var_llvm,
                                                             hi_llvm,
                                                             "for_test_condition");

  ir_builder->CreateCondBr(is_for_continue_condition, for_body_bb, for_next_bb);

  // for_body

  ir_builder->SetInsertPoint(for_body_bb);

  auto body_val_ty = this->body_->Translate(venv, tenv, level, errormsg);

  ir_builder->CreateBr(for_incre_bb);

  // 切出跳转分支

  ir_builder->SetInsertPoint(for_next_bb);


  venv->EndScope();

  /* 维护 loop stack */
  loop_stack.pop();

  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

tr::ValAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;

  auto out_loop_next_bb = loop_stack.top();

  ir_builder->CreateBr(out_loop_next_bb);

  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

tr::ValAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  venv->BeginScope();
  tenv->BeginScope();

  for (const auto &dec : this->decs_->GetList()) {
      dec->Translate(venv, tenv, level, errormsg);
  }

  auto ret_val_ty = this->body_->Translate(venv, tenv, level, errormsg);

  venv->EndScope();
  tenv->EndScope();

  return ret_val_ty;
}

/**
 * 返回llvm::Value*[element_type*]
 */
tr::ValAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  auto size_val_ty = this->size_->Translate(venv, tenv, level, errormsg);
  auto init_val_ty = this->init_->Translate(venv, tenv, level, errormsg);

  llvm::Value *init_value = nullptr;
  llvm::Value *size_value = size_val_ty->val_;

  if (size_value->getType()->isIntegerTy(1)){
      size_value = ir_builder->CreateZExt(size_value, getLLVMTypeInt32());
  }

  // FIXME: 如何保证得到的init_val_ty->val_ 必定要么是 int32 要么是 int64 ?
  if (init_val_ty->val_->getType()->isIntegerTy(1)){
      init_value = ir_builder->CreateZExt(init_val_ty->val_,
                                          getLLVMTypeInt64());
  } else if (init_val_ty->val_->getType()->isIntegerTy(32)){
      init_value = ir_builder->CreateSExt(init_val_ty->val_,
                                          getLLVMTypeInt64());
  } else {
      init_value = init_val_ty->val_;
  }

  auto array_addr_llvm_int64 = ir_builder->CreateCall(init_array,
                                                      {size_value,
                                                       init_value});

  auto type_entry = tenv->Look(this->typ_);

  // element_type *
  auto array_addr_llvm = ir_builder->CreateIntToPtr(array_addr_llvm_int64,
                                                    type_entry->GetLLVMType());

  return new tr::ValAndTy(array_addr_llvm, type_entry->ActualTy());
}

tr::ValAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5-part1 code here */
  // return nullptr;
  return new tr::ValAndTy(nullptr, type::VoidTy::Instance());
}

} // namespace absyn