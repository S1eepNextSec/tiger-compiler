#include "tiger/escape/escape.h"
#include "tiger/absyn/absyn.h"

namespace esc {
void EscFinder::FindEscape() { absyn_tree_->Traverse(env_.get()); }
} // namespace esc

namespace absyn {

// 逃逸分析检查每个function scope中的变量是否必须存放在内存中
// 每个syntax tree node节点检查自己使用的已经定义的变量
// 如果该变量定义的static scope depth小于当前节点的depth
// 该变量必须存放在栈上 即escape
void AbsynTree::Traverse(esc::EscEnvPtr env) {
  /* TODO: Put your lab5-part1 code here */
  this->root_->Traverse(env, 0);
}

void SimpleVar::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */

  auto esc_env_entry_p = env->Look(this->sym_);
  
  if (esc_env_entry_p != nullptr && esc_env_entry_p->depth_ < depth){
    // 使用到父函数 static scope 中的变量

    *esc_env_entry_p->escape_ = true;
  }
}

void FieldVar::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */

  this->var_->Traverse(env, depth);

}

void SubscriptVar::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */

  this->var_->Traverse(env, depth);

  this->subscript_->Traverse(env, depth);
}

void VarExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */

  this->var_->Traverse(env, depth);

}

void NilExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void IntExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void StringExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void CallExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  for (const auto & arg : this->args_->GetList()){
      arg->Traverse(env,depth);
  }  
}

void OpExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->left_->Traverse(env, depth);
  this->right_->Traverse(env, depth);
}

void RecordExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  for (const auto & efield : fields_->GetList()){
      efield->exp_->Traverse(env,depth);
  }
}

void SeqExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  for (const auto & exp : this->seq_->GetList()){
      exp->Traverse(env, depth);
  }
}

void AssignExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->var_->Traverse(env, depth);
  this->exp_->Traverse(env, depth);
}

void IfExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->test_->Traverse(env, depth);
  this->then_->Traverse(env,depth);
  // this->elsee_->Traverse(env, depth);
  if (this->elsee_){
      this->elsee_->Traverse(env, depth);
  }
}

void WhileExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->test_->Traverse(env, depth);
  this->body_->Traverse(env,depth);
}

void ForExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->lo_->Traverse(env, depth);
  this->hi_->Traverse(env, depth);

  env->BeginScope();

  this->escape_ = false;

  env->Enter(this->var_, new esc::EscapeEntry(depth, & (this->escape_)));

  this->body_->Traverse(env, depth);

  env->EndScope();
}

void BreakExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void LetExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  env->BeginScope();

  for (const auto & dec: this->decs_->GetList()){
      dec->Traverse(env, depth);
  }

  this->body_->Traverse(env, depth);

  env->EndScope();
}

void ArrayExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */

  this->size_->Traverse(env, depth);
  this->init_->Traverse(env, depth);
}

void VoidExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void FunctionDec::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  for (const auto & fun_dec : this->functions_->GetList()){
    env->BeginScope();

    for (const auto & field : fun_dec->params_->GetList()){
      field->escape_ = false;

      env->Enter(field->name_, new esc::EscapeEntry(depth + 1, & field->escape_));
    }

    fun_dec->body_->Traverse(env, depth + 1);

    env->EndScope();
  }
}

void VarDec::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->init_->Traverse(env, depth);

  env->Enter(this->var_, new esc::EscapeEntry(depth, & this->escape_));
}

void TypeDec::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

} // namespace absyn
