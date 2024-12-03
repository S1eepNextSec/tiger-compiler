#include "tiger/env/env.h"
#include "tiger/semant/semant.h"
#include "tiger/translate/translate.h"

extern llvm::IRBuilder<> *ir_builder;
extern llvm::Module *ir_module;

namespace sem {
void ProgSem::FillBaseTEnv() {
  tenv_->Enter(sym::Symbol::UniqueSymbol("int"), type::IntTy::Instance());
  tenv_->Enter(sym::Symbol::UniqueSymbol("string"), type::StringTy::Instance());
}

void ProgSem::FillBaseVEnv() {
  type::Ty *result;
  type::TyList *formals;

  venv_->Enter(sym::Symbol::UniqueSymbol("flush"),
               new env::FunEntry(new type::TyList(), type::VoidTy::Instance()));

  formals = new type::TyList(type::IntTy::Instance());

  venv_->Enter(sym::Symbol::UniqueSymbol("exit"),
               new env::FunEntry(formals, type::VoidTy::Instance()));

  result = type::StringTy::Instance();

  venv_->Enter(sym::Symbol::UniqueSymbol("chr"),
               new env::FunEntry(formals, result));

  venv_->Enter(sym::Symbol::UniqueSymbol("getchar"),
               new env::FunEntry(new type::TyList(), result));

  formals = new type::TyList(type::StringTy::Instance());

  venv_->Enter(sym::Symbol::UniqueSymbol("print"),
               new env::FunEntry(formals, type::VoidTy::Instance()));
  venv_->Enter(sym::Symbol::UniqueSymbol("printi"),
               new env::FunEntry(new type::TyList(type::IntTy::Instance()),
                                 type::VoidTy::Instance()));

  result = type::IntTy::Instance();
  venv_->Enter(sym::Symbol::UniqueSymbol("ord"),
               new env::FunEntry(formals, result));

  venv_->Enter(sym::Symbol::UniqueSymbol("size"),
               new env::FunEntry(formals, result));

  result = type::StringTy::Instance();
  formals = new type::TyList(
      {type::StringTy::Instance(), type::StringTy::Instance()});
  venv_->Enter(sym::Symbol::UniqueSymbol("concat"),
               new env::FunEntry(formals, result));

  formals =
      new type::TyList({type::StringTy::Instance(), type::IntTy::Instance(),
                        type::IntTy::Instance()});
  venv_->Enter(sym::Symbol::UniqueSymbol("substring"),
               new env::FunEntry(formals, result));
}

} // namespace sem

namespace tr {

void ProgTr::FillBaseTEnv() {
  tenv_->Enter(sym::Symbol::UniqueSymbol("int"), type::IntTy::Instance());
  tenv_->Enter(sym::Symbol::UniqueSymbol("string"), type::StringTy::Instance());
}

void ProgTr::FillBaseVEnv() {
  type::Ty *result;
  type::TyList *formals;

  tr::Level *level = main_level_.get();

  venv_->Enter(sym::Symbol::UniqueSymbol("flush"),
               new env::FunEntry(level, new type::TyList(),
                                 type::VoidTy::Instance(), "flush"));

  formals = new type::TyList(type::IntTy::Instance());

  venv_->Enter(
      sym::Symbol::UniqueSymbol("exit"),
      new env::FunEntry(level, formals, type::VoidTy::Instance(), "exit"));

  result = type::StringTy::Instance();

  venv_->Enter(sym::Symbol::UniqueSymbol("chr"),
               new env::FunEntry(level, formals, result, "chr"));

  venv_->Enter(
      sym::Symbol::UniqueSymbol("getchar"),
      new env::FunEntry(level, new type::TyList(), result, "__wrap_getchar"));

  formals = new type::TyList(type::StringTy::Instance());

  venv_->Enter(
      sym::Symbol::UniqueSymbol("print"),
      new env::FunEntry(level, formals, type::VoidTy::Instance(), "print"));
  venv_->Enter(sym::Symbol::UniqueSymbol("printi"),
               new env::FunEntry(level,
                                 new type::TyList(type::IntTy::Instance()),
                                 type::VoidTy::Instance(), "printi"));

  result = type::IntTy::Instance();
  venv_->Enter(sym::Symbol::UniqueSymbol("ord"),
               new env::FunEntry(level, formals, result, "ord"));

  venv_->Enter(sym::Symbol::UniqueSymbol("size"),
               new env::FunEntry(level, formals, result, "size"));

  result = type::StringTy::Instance();
  formals = new type::TyList(
      {type::StringTy::Instance(), type::StringTy::Instance()});
  venv_->Enter(sym::Symbol::UniqueSymbol("concat"),
               new env::FunEntry(level, formals, result, "concat"));

  formals =
      new type::TyList({type::StringTy::Instance(), type::IntTy::Instance(),
                        type::IntTy::Instance()});
  venv_->Enter(sym::Symbol::UniqueSymbol("substring"),
               new env::FunEntry(level, formals, result, "substring"));
}

} // namespace tr

/**
 * FunEntry构造函数
 * 同时生成LLVM中的函数
 */
env::FunEntry::FunEntry(tr::Level *level, type::TyList *formals,
                        type::Ty *result, std::string func_name)
    : formals_(formals), result_(result), level_(level) {
  std::vector<llvm::Type *> formals_ty;
  for (auto arg_ty : formals->GetList()) {
    formals_ty.push_back(arg_ty->GetLLVMType());
  }
  llvm::FunctionType *func_type =
      llvm::FunctionType::get(result->GetLLVMType(), formals_ty, false);
  llvm::Function *func = llvm::Function::Create(
      func_type, llvm::Function::ExternalLinkage, func_name, ir_module);
  func_type_ = func_type;
  func_ = func;
}
