#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"

namespace absyn {

void AbsynTree::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                           err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  // 直接对AST进行语义分析
  this->root_->SemAnalyze(venv, tenv, 0, errormsg);
}

type::Ty *SimpleVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // search var entry in var table
  auto var_type = venv->Look(this->sym_);

  auto var_type_derived = dynamic_cast<env::VarEntry *>(var_type);

  if (var_type_derived == nullptr){
      errormsg->Error(this->pos_, "undefined variable %s", this->sym_->Name().data());

      return type::VoidTy::Instance();//avoid using nullptr
  }

  return var_type_derived->ty_;
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // var must be a record type
  auto var_type = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);

  auto record_type = dynamic_cast<type::RecordTy *>(var_type->ActualTy());

  if (record_type == nullptr){
      errormsg->Error(this->pos_, "not a record type");
      return type::VoidTy::Instance();  //avoid using nullptr, which will cause segfault
  }

  for (const auto & field : record_type->fields_->GetList()){
    if (field->name_ == this->sym_){
        return field->ty_->ActualTy();
    } 
  }

  errormsg->Error(this->pos_, "field %s doesn't exist",this->sym_->Name().data());

  return type::VoidTy::Instance();  // no corresponding field , choose to return a void
}

type::Ty *SubscriptVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   int labelcount,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  auto index_exp_type = this->subscript_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();

  auto array_type = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();

  // index must be an integer
  if (index_exp_type != type::IntTy::Instance()){
      errormsg->Error(this->pos_, "index in subscipt var should be int");
  }
  
  // type of var must be an array type
  auto array_type_derived = dynamic_cast<type::ArrayTy *>(array_type->ActualTy());

  if (array_type_derived == nullptr){
      errormsg->Error(this->pos_, "array type required");
      return type::VoidTy::Instance();  // avoid using nullptr , directly return
  }

  auto element_type = array_type_derived->ty_->ActualTy();

  return element_type->ActualTy();
}

type::Ty *VarExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return this->var_->SemAnalyze(venv,tenv,labelcount,errormsg)->ActualTy();
}

type::Ty *NilExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::NilTy::Instance();
}

type::Ty *IntExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::IntTy::Instance();
}

type::Ty *StringExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::StringTy::Instance();
}

type::Ty *CallExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // corresponding function must exist
  auto fun_entry = venv->Look(func_);

  if (fun_entry == nullptr){
      errormsg->Error(this->pos_, "undefined function %s", func_->Name().data());
      return type::VoidTy::Instance();
  }

  auto fun_entry_derived = dynamic_cast<env::FunEntry *>(fun_entry);

  if (fun_entry_derived == nullptr){
      errormsg->Error(this->pos_, "function required");
      return type::VoidTy::Instance();  // avoid using nullptr
  }

  // check num of params
  if (fun_entry_derived->formals_->GetList().size() < this->args_->GetList().size()){
      errormsg->Error(this->pos_, "too many params in function %s",this->func_->Name().data());
      return fun_entry_derived->result_;
  }

  if (fun_entry_derived->formals_->GetList().size() > this->args_->GetList().size()) {
      errormsg->Error(this->pos_, "para type mismatch");
      return fun_entry_derived->result_;
  }
  
  auto expect_param_type_it = fun_entry_derived->formals_->GetList().begin();

  auto call_param_type_it = this->args_->GetList().begin();

  // check type one by one
  while (expect_param_type_it != fun_entry_derived->formals_->GetList().end()){
    //FIXME: 注意到venv 中 FunctionEntry 的 Formal list中的Ty* 可能含有nullptr
      if ((*expect_param_type_it) == nullptr || !(*expect_param_type_it)->IsSameType((*call_param_type_it)->SemAnalyze(venv, tenv, labelcount, errormsg))) {
          errormsg->Error(this->pos_, "para type mismatch");
      }

    expect_param_type_it++;
    call_param_type_it++;
  }

  return fun_entry_derived->result_;
}

type::Ty *OpExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  auto left_type = this->left_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto right_type = this->right_->SemAnalyze(venv, tenv, labelcount, errormsg);
  
  // numerical option requires that both exp returns integer
  if (this->oper_ == Oper::PLUS_OP || this->oper_ == Oper::MINUS_OP ||
      this->oper_ == Oper::DIVIDE_OP || this->oper_ == Oper::TIMES_OP ||
      this->oper_ == Oper::AND_OP || this->oper_ == Oper::OR_OP){
        
        if (left_type->ActualTy() != type::IntTy::Instance() || right_type->ActualTy() !=type::IntTy::Instance()){
            errormsg->Error(this->pos_, "integer required");
        }
        return type::IntTy::Instance();
  }

  // must be the same type
  if (!left_type->IsSameType(right_type)) {
      errormsg->Error(this->pos_, "same type required");
  }
  
  return type::IntTy::Instance();
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // corresponding record type must exist in tenv table
  auto expected_type = tenv->Look(this->typ_);

  if (expected_type == nullptr){
      errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
      return type::VoidTy::Instance();  // avoid using nullptr to analyze
  }

  auto record_type = dynamic_cast<type::RecordTy *>(expected_type->ActualTy());

  if (record_type == nullptr){
      errormsg->Error(this->pos_, "record type required");
      return type::VoidTy::Instance();  // avoid using nullptr to analyze
  }

  // size check
  if (record_type->fields_->GetList().size() != this->fields_->GetList().size()){
      errormsg->Error(this->pos_, "size of fields mismatch");
  }

  auto absyn_list_it = this->fields_->GetList().begin();

  auto semant_list_it = record_type->fields_->GetList().begin();

  while (absyn_list_it != this->fields_->GetList().end()){
    if ((*absyn_list_it)->name_ != (*semant_list_it)->name_){
        errormsg->Error(this->pos_, "field unmatched");
    }

    if (!(*absyn_list_it)->exp_->SemAnalyze(venv, tenv, labelcount, errormsg)->IsSameType((*semant_list_it)->ty_)) {
        errormsg->Error(this->pos_, "same type required");
    }

    absyn_list_it++;
    semant_list_it++;
  }

  return record_type;
}


type::Ty *SeqExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // return the last exp type
  type::Ty *last_exp_type = type::VoidTy::Instance();

  for (const auto & exp : this->seq_->GetList()){
      auto exp_type = exp->SemAnalyze(venv, tenv, labelcount, errormsg);

      last_exp_type = exp_type;
  }

  return last_exp_type;
}

type::Ty *AssignExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // if it is a simple var
  auto simple_var = dynamic_cast<SimpleVar *>(this->var_);

  if (simple_var != nullptr){
      auto entry = venv->Look(simple_var->sym_);

      auto var_entry = dynamic_cast<env::EnvEntry *>(entry);

      if (var_entry == nullptr){
          errormsg->Error(this->pos_, "lvalue required");
      }

      if (var_entry->readonly_){
          errormsg->Error(this->pos_, "loop variable can't be assigned");
      }
  }

  auto var_type = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);

  auto exp_type = this->exp_->SemAnalyze(venv, tenv, labelcount, errormsg);

  if (!var_type->IsSameType(exp_type)) {
      errormsg->Error(this->pos_, "unmatched assign exp");
  }

  return type::VoidTy::Instance();
}

type::Ty *IfExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  if (this->elsee_ != nullptr){
      auto else_exp_type = this->elsee_->SemAnalyze(venv, tenv, labelcount, errormsg);
      auto then_exp_type = this->then_->SemAnalyze(venv, tenv, labelcount, errormsg);
      auto test_exp_type = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);

      //test condition must return integer
      if (test_exp_type->ActualTy() != type::IntTy::Instance()){
          errormsg->Error(this->pos_, "test expression in if expression should return int type");   
      }

      // type should match
      if (!then_exp_type->IsSameType(else_exp_type)) {
          errormsg->Error(this->pos_, "then exp and else exp type mismatch");
      }

      return then_exp_type;
  } else {
      auto then_exp_type = this->then_->SemAnalyze(venv, tenv, labelcount, errormsg);
      auto test_exp_type = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);

      // test condition should return integer
      if (test_exp_type->ActualTy() != type::IntTy::Instance()) {
          errormsg->Error(this->pos_, "test expression in if expression should return int type");
      }

      // return no value
      if (then_exp_type != type::VoidTy::Instance()){
          errormsg->Error(this->pos_, "if-then exp's body must produce no value");
      }

      return type::VoidTy::Instance();
  }
}

type::Ty *WhileExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  // while exp do exp
  auto condition_type = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);

  // test condition should return integer type
  if (condition_type->ActualTy() != type::IntTy::Instance()){
      errormsg->Error(this->pos_, "type of the result of the test expression in a while expression should be int");
  }

  auto body_type = this->body_->SemAnalyze(venv, tenv, labelcount + 1, errormsg);
  
  // while return no value
  if (body_type->ActualTy() != type::VoidTy::Instance()){
      errormsg->Error(this->pos_, "while body must produce no value");
  }

  return type::VoidTy::Instance();
}

type::Ty *ForExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // for id := exp to exp do exp
  venv->BeginScope();

  auto low_bound_type = this->lo_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto high_bound_type = this->hi_->SemAnalyze(venv, tenv, labelcount, errormsg);
  
  // bound should be integer
  if (low_bound_type->ActualTy() != type::IntTy::Instance() || high_bound_type->ActualTy() != type::IntTy::Instance()){
      errormsg->Error(this->pos_, "for exp's range type is not integer");
  }
  
  // loop var must be readonly
  venv->Enter(this->var_, new env::VarEntry(type::IntTy::Instance(),true));

  this->body_->SemAnalyze(venv,tenv,labelcount + 1,errormsg);

  venv->EndScope();
  
  // for exp returns no value
  return type::VoidTy::Instance();
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  if (labelcount == 0){
      errormsg->Error(this->pos_, "break exp must be inside a loop");
  }

  return type::VoidTy::Instance();
}

type::Ty *LetExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  // let decs in seqs end
  venv->BeginScope();
  tenv->BeginScope();

  for (const auto & dec : this->decs_->GetList()){
      dec->SemAnalyze(venv, tenv, labelcount, errormsg);
  }

  auto result_type = this->body_->SemAnalyze(venv, tenv, labelcount, errormsg);

  venv->EndScope();
  tenv->EndScope();

  return result_type;
}

type::Ty *ArrayExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // array-type [ exp ] of exp

  auto size_exp_type = this->size_->SemAnalyze(venv, tenv, labelcount, errormsg);

  auto init_exp_type = this->init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  
  // type must be an array type
  auto array_type = tenv->Look(this->typ_);

  type::ArrayTy * array_type_derived = dynamic_cast<type::ArrayTy *>(array_type->ActualTy());

  if (array_type_derived == nullptr){
      errormsg->Error(this->pos_, "array type required");
      return type::VoidTy::Instance();  // avoid using nullptr , following execution will fetch element_type
  }

  auto element_type = array_type_derived->ty_;

  if (!element_type->IsSameType(init_exp_type)) {
      errormsg->Error(this->pos_, "type mismatch");
  }

  if (size_exp_type->ActualTy() != type::IntTy::Instance()){
      errormsg->Error(this->pos_, "type of the size of an array should be int");
  }

  return array_type;
}

type::Ty *VoidExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::VoidTy::Instance();
}

void FunctionDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  std::unordered_map<sym::Symbol *, env::FunEntry *> fun_name_entry_map;
  std::unordered_set<absyn::FunDec *> skip_fun_dec;
  
  // add header to venv
  for (const auto &absyn_fun_dec : this->functions_->GetList()) {
      auto exist_entry = venv->Look(absyn_fun_dec->name_);

      if (exist_entry != nullptr){
          errormsg->Error(this->pos_,"two functions have the same name");
          skip_fun_dec.insert(absyn_fun_dec);
          continue;
      }

      // ty_list might contain nullptr!!!!!
      // undefined behaviour? segfault...
      auto ty_list = absyn_fun_dec->params_->MakeFormalTyList(tenv, errormsg);

      auto fun_entry = new env::FunEntry(ty_list, type::VoidTy::Instance());

      if (absyn_fun_dec->result_ != nullptr){
          auto return_type = tenv->Look(absyn_fun_dec->result_);

          // return type might be nullptr !!!

          if (return_type == nullptr){
              errormsg->Error(this->pos_, "undefined type %s", absyn_fun_dec->result_->Name().data());
          } else {
            fun_entry->result_ = return_type;
          }
      }
      // ensures that result ty won't be nullptr

      venv->Enter(absyn_fun_dec->name_, fun_entry);

      fun_name_entry_map.insert({absyn_fun_dec->name_, fun_entry});
  }

  for (const auto & absyn_fun_dec : this->functions_->GetList()){
    if (skip_fun_dec.find(absyn_fun_dec) != skip_fun_dec.end()){
        continue;
    }
    // add params to a local scope
    venv->BeginScope();

    auto fun_entry_it = fun_name_entry_map.find(absyn_fun_dec->name_);

    auto fun_entry = fun_entry_it->second;

    auto absyn_param_it = absyn_fun_dec->params_->GetList().begin();
    auto semant_param_it = fun_entry->formals_->GetList().begin();

    while(absyn_param_it != absyn_fun_dec->params_->GetList().end()){
        // notice that the Ty* in formals_ may be nullptr!
        // venv->Enter((*absyn_param_it)->name_, new env::VarEntry((*semant_param_it)->ActualTy()));
        //FIXME: formals的生成依赖于MakeFormal的函数 函数中如果未在tenv中找到对应类型会直接在列表中加入nullptr
        //       nullptr 意味着这可能在函数的scope内 venv表中有一个var entry的Ty* 为nullptr
        //       这里的解决方案暂时为将Ty* 设为 VoidTy
        //       另一种方法是设置各种 absyn中的Var 检测在venv中获取到entry时 Ty* 是否为nullptr 是的话返回某个特定类
        if (*semant_param_it == nullptr){
            venv->Enter((*absyn_param_it)->name_, new env::VarEntry(type::VoidTy::Instance()));
        } else {
            venv->Enter((*absyn_param_it)->name_, new env::VarEntry((*semant_param_it)->ActualTy()));
        }

        absyn_param_it++;
        semant_param_it++;
    }

    auto result_type = absyn_fun_dec->body_->SemAnalyze(venv, tenv, labelcount, errormsg);

    if (result_type->ActualTy() != type::VoidTy::Instance() && fun_entry->result_->ActualTy() == type::VoidTy::Instance()){
        errormsg->Error(this->pos_, "procedure returns value");
    }

    venv->EndScope();
  }
  
}

void VarDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                        err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // VAR ID := exp / VAR ID : type := exp

  auto exp_type = this->init_->SemAnalyze(venv,tenv,labelcount,errormsg);

  if (this->typ_ != nullptr){
      auto var_type = tenv->Look(this->typ_);

      // var_type may be nullptr
      if (var_type == nullptr){
          errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
      }

      if (var_type && !var_type->IsSameType(exp_type)) {
          errormsg->Error(this->pos_, "type mismatch");
      }
  }

  if (this->typ_ == nullptr && exp_type->ActualTy() == type::NilTy::Instance()){
      errormsg->Error(this->pos_, "init should not be nil without type specified");
  }

  // ensures that exp_type will never be nullptr
  venv->Enter(this->var_, new env::VarEntry(exp_type->ActualTy()));
}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  std::list<type::NameTy*> indirect_types;
  std::unordered_set<absyn::NameAndTy *> skip_types;

  // type ID = array of ID / ID / { ID : ID, ID : ID ...}
  // add header to tenv
  for (const auto & absyn_name_ty_ptr : this->types_->GetList()){
      auto entry = tenv->Look(absyn_name_ty_ptr->name_);

      if (entry != nullptr){
          errormsg->Error(this->pos_, "two types have the same name");
          skip_types.insert(absyn_name_ty_ptr);
          continue;
      }

      auto ty = new type::NameTy(absyn_name_ty_ptr->name_, nullptr);

      tenv->Enter(absyn_name_ty_ptr->name_, ty);
  }

  for (const auto & absyn_name_ty_ptr : this->types_->GetList()){
    if (skip_types.find(absyn_name_ty_ptr) != skip_types.end()){
        // do not analyze duplicated type dec
        continue;
    }

      auto l_type = tenv->Look(absyn_name_ty_ptr->name_);

      auto l_type_derived = dynamic_cast<type::NameTy *>(l_type);

      // what will happen if r_type gets a nullptr?
      // in type::NameTy & type::ArrayTy & type::RecordTy ensures th at 
      // when analyze a absyn::Ty won't get a nullptr
      auto r_type = absyn_name_ty_ptr->ty_->SemAnalyze(tenv,errormsg);

      // NameTy Case
      auto r_name_ty_derived = dynamic_cast<type::NameTy *>(r_type);

      if (r_name_ty_derived != nullptr) {
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

      while ((type_to_check = dynamic_cast<type::NameTy*>(type_to_check->ty_))!= nullptr){
          if (set.find(type_to_check) != set.end()){
              errormsg->Error(this->pos_, "illegal type cycle");
              isCycleFound = true;
              break;
          }
          set.insert(type_to_check);
      }

      if (isCycleFound){
          break;
      }
  }
}

// type a = b(absyn::NameTy)
// type b = int(absyn::NameTy)
// type c = array of b(absyn::ArrayTy)
// type d = {f_1:a , f_2:b}(absyn::RecordTy)

type::Ty *NameTy::SemAnalyze(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  auto type = tenv->Look(this->name_);

  if (type == nullptr){
      errormsg->Error(this->pos_, "undefined type %s", this->name_->Name().data());

      // undefined type , default void
      type = type::VoidTy::Instance();
  }
  // transfer

  // type won't be a nullptr 
  // it's pretty safe
  return type;
}

type::Ty *RecordTy::SemAnalyze(env::TEnvPtr tenv,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // transfer
  type::FieldList *type_field_list = new type::FieldList();

  for (const auto & absyn_field : this->record_->GetList()){
    const auto & field_name = absyn_field->name_;
    const auto & type_name = absyn_field->typ_;

    auto type = tenv->Look(type_name);

    if (type == nullptr){
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

type::Ty *ArrayTy::SemAnalyze(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // get element type in tenv
  auto element_type = tenv->Look(this->array_);

  if (element_type == nullptr){
      errormsg->Error(this->pos_, "undefined type %s", this->array_->Name().data());

      // default set the element_type to void type
      element_type = type::VoidTy::Instance();
  }

  // transfer Ty in Absyn Tree to 
  //          Ty in type used in semantic analysis

  // now the return type will not contain nullptr in it
  // it's pretty safe

  return new type::ArrayTy(element_type);
}

} // namespace absyn

namespace sem {

void ProgSem::SemAnalyze() {
  FillBaseVEnv();
  FillBaseTEnv();
  absyn_tree_->SemAnalyze(venv_.get(), tenv_.get(), errormsg_.get());
}
} // namespace sem
