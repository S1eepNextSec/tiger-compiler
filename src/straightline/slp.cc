#include "straightline/slp.h"

#include <iostream>

namespace A {
int A::CompoundStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  int stm1args = stm1->MaxArgs();
  int stm2args = stm2->MaxArgs();

  return stm1args > stm2args ? stm1args : stm2args;
}

Table *A::CompoundStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  return stm2->Interp(stm1->Interp(t));
}

int A::AssignStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return this->exp->MaxArgs();
}

Table *A::AssignStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  IntAndTable *it = exp->Interp(t);

  return it->t->Update(this->id, it->i);
}

int A::PrintStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  int subMaxArgs = this->exps->MaxArgs();
  int args = this->exps->NumExps();

  return subMaxArgs > args ? subMaxArgs : args;
}

Table *A::PrintStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  IntAndTable *it = this->exps->Interp(t);
  return it->t;
}

int IdExp::MaxArgs()const{
    return 0;
}
int NumExp::MaxArgs()const{
    return 0;
}
int OpExp::MaxArgs()const{
    int leftMaxArgs = this->left->MaxArgs();
    int rightMaxArgs = this->right->MaxArgs();
    return leftMaxArgs > rightMaxArgs ? leftMaxArgs : rightMaxArgs;
}
int EseqExp::MaxArgs() const
{
    int stmMaxArgs = stm->MaxArgs();
    int expMaxArgs = exp->MaxArgs();

    return stmMaxArgs > expMaxArgs ? stmMaxArgs : expMaxArgs;
}

IntAndTable* IdExp::Interp(Table *t) const{
  int val = t->Lookup(this->id);
  return new IntAndTable(val, t);
}

IntAndTable* NumExp::Interp(Table *t) const{
  return new IntAndTable(this->num, t);
}

IntAndTable* OpExp::Interp(Table *t) const{
    IntAndTable* it1 = this->left->Interp(t);
    IntAndTable* it2 = this->right->Interp(it1->t);

    int val = 0;
    
    switch (this->oper) {
    case PLUS:
        val = it1->i + it2->i;
        break;
    case MINUS:
        val = it1->i - it2->i;
        break;
    case TIMES:
        val = it1->i * it2->i;
        break;
    case DIV:
        val = it1->i / it2->i;
        break;
    }
    return new IntAndTable(val, it2->t);
}

IntAndTable* EseqExp::Interp(Table *t) const{
    Table *t1 = this->stm->Interp(t);

    IntAndTable* it = this->exp->Interp(t1);

    return it;
}


int PairExpList::MaxArgs() const {
    int expMaxArgs = this->exp->MaxArgs();
    int restMaxArgs = this->tail->MaxArgs();

    return expMaxArgs > restMaxArgs ? expMaxArgs : restMaxArgs;
}

int LastExpList::MaxArgs() const {
    return this->exp->MaxArgs();
}

int PairExpList::NumExps() const {
    return 1 + this->tail->NumExps();
}

int LastExpList::NumExps() const {
    return 1;
}

IntAndTable* PairExpList::Interp(Table * t)const{
    IntAndTable *it1 = this->exp->Interp(t);

    printf("%d ",it1->i);

    IntAndTable *it2 = this->tail->Interp(it1->t);
    return it2;
}

IntAndTable* LastExpList::Interp(Table* t)const{
    IntAndTable *it = this->exp->Interp(t);

    printf("%d\n",it->i);
    return it;
}

int Table::Lookup(const std::string &key) const {
  if (id == key) {
    return value;
  } else if (tail != nullptr) {
    return tail->Lookup(key);
  } else {
    assert(false);
  }
}

Table *Table::Update(const std::string &key, int val) const {
  return new Table(key, val, this);
}
}  // namespace A
