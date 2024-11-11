#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"

namespace absyn {

void AbsynTree::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                           err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  this->root_->SemAnalyze(venv, tenv, 0, errormsg);
}

type::Ty *SimpleVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  auto var_type = venv->Look(this->sym_);

  auto var_type_derived = dynamic_cast<env::VarEntry *>(var_type);

  if (var_type_derived == nullptr){
      errormsg->Error(this->pos_, "undefined variable %s", this->sym_->Name().data());
  }

  return var_type_derived->ty_->ActualTy();
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  auto var_type = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);

  auto record_type = dynamic_cast<type::RecordTy *>(var_type->ActualTy());

  if (record_type == nullptr){
      errormsg->Error(this->pos_, "not a record type");
      return type::VoidTy::Instance();
  }

  for (const auto & field : record_type->fields_->GetList()){
    if (field->name_ == this->sym_){
        return field->ty_->ActualTy();
    } 
  }

  errormsg->Error(this->pos_, "field %s doesn't exist",this->sym_->Name().data());

  return type::VoidTy::Instance();
}

type::Ty *SubscriptVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   int labelcount,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  auto index_exp_type = this->subscript_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();

  auto array_type = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg)->ActualTy();

  if (index_exp_type != type::IntTy::Instance()){
      errormsg->Error(this->pos_, "index in subscipt var should be int");
  }

  auto array_type_derived = dynamic_cast<type::ArrayTy *>(array_type->ActualTy());

  if (array_type_derived == nullptr){
      errormsg->Error(this->pos_, "array type required");
      return type::VoidTy::Instance();  // 防止 后续分析使用 nullptr
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
  auto fun_entry = venv->Look(func_);

  if (fun_entry == nullptr){
      errormsg->Error(this->pos_, "undefined function %s", func_->Name().data());
      return type::VoidTy::Instance();
  }

  auto fun_entry_derived = dynamic_cast<env::FunEntry *>(fun_entry);

  if (fun_entry_derived == nullptr){
      errormsg->Error(this->pos_, "function required");
      return type::VoidTy::Instance();
  }

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

  while (expect_param_type_it != fun_entry_derived->formals_->GetList().end()){
      if (!(*expect_param_type_it)->IsSameType((*call_param_type_it)->SemAnalyze(venv, tenv, labelcount, errormsg))) {
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
  
  if (this->oper_ == Oper::PLUS_OP || this->oper_ == Oper::MINUS_OP ||
      this->oper_ == Oper::DIVIDE_OP || this->oper_ == Oper::TIMES_OP ||
      this->oper_ == Oper::AND_OP || this->oper_ == Oper::OR_OP){
        
        if (left_type->ActualTy() != type::IntTy::Instance() || right_type->ActualTy() !=type::IntTy::Instance()){
            errormsg->Error(this->pos_, "integer required");
        }
        return type::IntTy::Instance();
  }

  if (!left_type->IsSameType(right_type)) {
      errormsg->Error(this->pos_, "same type required");
  }

  return type::IntTy::Instance();
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  auto expected_type = tenv->Look(this->typ_);

  if (expected_type == nullptr){
      errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
      return type::VoidTy::Instance();
  }

  auto record_type = dynamic_cast<type::RecordTy *>(expected_type->ActualTy());

  if (record_type == nullptr){
      errormsg->Error(this->pos_, "given type is not a record type in a record expression");
      return type::VoidTy::Instance();
  }

  if (record_type->fields_->GetList().size() != this->fields_->GetList().size()){
      errormsg->Error(this->pos_, "size of field list failed to match");
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
      //TODO: labelcount
      auto else_exp_type = this->elsee_->SemAnalyze(venv, tenv, labelcount, errormsg);
      auto then_exp_type = this->then_->SemAnalyze(venv, tenv, labelcount, errormsg);
      auto test_exp_type = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);

      if (test_exp_type->ActualTy() != type::IntTy::Instance()){
          errormsg->Error(this->pos_, "test expression in if expression should return int type");   
      }

      if (!then_exp_type->IsSameType(else_exp_type)) {
          errormsg->Error(this->pos_, "then exp and else exp type mismatch");
      }

      return then_exp_type;
  } else {
      auto then_exp_type = this->then_->SemAnalyze(venv, tenv, labelcount, errormsg);
      auto test_exp_type = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);

      if (test_exp_type->ActualTy() != type::IntTy::Instance()) {
          errormsg->Error(this->pos_, "test expression in if expression should return int type");
      }

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
  //TODO: labelcount
  auto condition_type = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);

  if (condition_type->ActualTy() != type::IntTy::Instance()){
      errormsg->Error(this->pos_, "type of the result of the test expression in a while expression should be int");
  }

  //TODO:labelcount
  auto body_type = this->body_->SemAnalyze(venv, tenv, labelcount, errormsg);

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

  //TODO: labelcount
  auto low_bound_type = this->lo_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto high_bound_type = this->hi_->SemAnalyze(venv, tenv, labelcount, errormsg);

  if (low_bound_type->ActualTy() != type::IntTy::Instance() || high_bound_type->ActualTy() != type::IntTy::Instance()){
      errormsg->Error(this->pos_, "for exp's range type is not integer");
  }

  //TODO: readonly?
  venv->Enter(this->var_, new env::VarEntry(type::IntTy::Instance(),true));

  //TODO:labelcount?
  this->body_->SemAnalyze(venv,tenv,labelcount,errormsg);

  venv->EndScope();

  return type::VoidTy::Instance();
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // TODO: maybe something wrong?
  // labelcount ?

  return type::VoidTy::Instance();
}

type::Ty *LetExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  // let decs in seqs end
  venv->BeginScope();
  tenv->BeginScope();

  for (const auto & dec : this->decs_->GetList()){
    //TODO: what does labelcount mean?
      dec->SemAnalyze(venv, tenv, labelcount, errormsg);
  }

  // TODO: what does labelcount mean?
  auto result_type = this->body_->SemAnalyze(venv, tenv, labelcount, errormsg);

  venv->EndScope();
  tenv->EndScope();

  // TODO: maybe something wrong?
  return result_type;
}

type::Ty *ArrayExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // array-type [ exp ] of exp

  //TODO: what does labelcount mean?
  auto index_exp_type = this->size_->SemAnalyze(venv, tenv, labelcount, errormsg);

  //TODO: what does labelcount mean?
  auto init_exp_type = this->init_->SemAnalyze(venv, tenv, labelcount, errormsg);

  auto array_type = tenv->Look(this->typ_);

  type::ArrayTy * array_type_derived = dynamic_cast<type::ArrayTy *>(array_type->ActualTy());

  if (array_type_derived == nullptr){
      errormsg->Error(this->pos_, "array type required");
      return type::VoidTy::Instance();
  }

  auto element_type = array_type_derived->ty_;

  if (!element_type->IsSameType(init_exp_type)) {
      errormsg->Error(this->pos_, "type mismatch");
  }

  if (index_exp_type->ActualTy() != type::IntTy::Instance()){
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

  for (const auto &absyn_fun_dec : this->functions_->GetList()) {
      auto exist_entry = venv->Look(absyn_fun_dec->name_);

      if (exist_entry != nullptr){
          errormsg->Error(this->pos_,"two functions have the same name");
          skip_fun_dec.insert(absyn_fun_dec);
          continue;
      }

      auto ty_list = absyn_fun_dec->params_->MakeFormalTyList(tenv, errormsg);

      auto fun_entry = new env::FunEntry(ty_list, type::VoidTy::Instance());

      if (absyn_fun_dec->result_ != nullptr){
          auto return_type = tenv->Look(absyn_fun_dec->result_);

          fun_entry->result_ = return_type->ActualTy();
      }

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
        venv->Enter((*absyn_param_it)->name_, new env::VarEntry((*semant_param_it)->ActualTy()));

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

  // what do labelcount do ?
  auto exp_type = this->init_->SemAnalyze(venv,tenv,labelcount,errormsg);

  if (this->typ_ != nullptr){
      auto var_type = tenv->Look(this->typ_);

      if (!var_type->IsSameType(exp_type)) {
          errormsg->Error(this->pos_, "type mismatch");
      }
  }

  if (this->typ_ == nullptr && exp_type->ActualTy() == type::NilTy::Instance()){
      errormsg->Error(this->pos_, "init should not be nil without type specified");
  }

  venv->Enter(this->var_, new env::VarEntry(exp_type->ActualTy()));
}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  std::list<type::NameTy*> indirect_types;

  // type ID = array of ID / ID / { ID : ID, ID : ID ...}
  for (const auto & absyn_name_ty_ptr : this->types_->GetList()){
      auto entry = tenv->Look(absyn_name_ty_ptr->name_);

      if (entry != nullptr){
          errormsg->Error(this->pos_, "two types have the same name");
          continue;
      }

      auto ty = new type::NameTy(absyn_name_ty_ptr->name_, nullptr);
      tenv->Enter(absyn_name_ty_ptr->name_, ty);
  }

  for (const auto & absyn_name_ty_ptr : this->types_->GetList()){
      auto l_type = tenv->Look(absyn_name_ty_ptr->name_);

      auto l_type_derived = dynamic_cast<type::NameTy *>(l_type);

      // what will happen if r_type gets a nullptr?
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

      // ?
      // origin->ty_ = origin->ActualTy();

      //what if the pointer finally a nullptr?
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
  }
  // transfer
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
    }

    type_field_list->Append(new type::Field(field_name, type));
  }

  return new type::RecordTy(type_field_list);
}

type::Ty *ArrayTy::SemAnalyze(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  // get element type in tenv
  auto element_type = tenv->Look(this->array_);

  if (element_type == nullptr){
      errormsg->Error(this->pos_, "undefined type %s", this->array_->Name().data());
  }

  // transfer Ty in Absyn Tree to 
  //          Ty in type used in semantic analysis

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
