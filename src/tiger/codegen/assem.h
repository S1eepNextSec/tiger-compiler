#ifndef TIGER_CODEGEN_ASSEM_H_
#define TIGER_CODEGEN_ASSEM_H_

#include <cstdio>
#include <string>
#include <vector>

#include "tiger/frame/temp.h"

namespace assem {

class Targets {
public:
  std::vector<temp::Label *> *labels_;

  explicit Targets(std::vector<temp::Label *> *labels) : labels_(labels) {}
};

/**
 * Instr 类用来抽象一条汇编指令
 */
class Instr {
public:
  virtual ~Instr() = default;

  virtual void Print(FILE *out, temp::Map *m) const = 0;
  [[nodiscard]] virtual temp::TempList *Def() const = 0;
  [[nodiscard]] virtual temp::TempList *Use() const = 0;
};

class OperInstr : public Instr {
public:
  std::string assem_; //  汇编指令文本
  temp::TempList *dst_, *src_;  //  指令设计的 src list & dst list
  Targets *jumps_;  // 跳转目标 包含则说明对应的指令为跳转指令

  OperInstr(std::string assem, temp::TempList *dst, temp::TempList *src,
            Targets *jumps)
      : assem_(std::move(assem)), dst_(dst), src_(src), jumps_(jumps) {}

  void Print(FILE *out, temp::Map *m) const override;
  [[nodiscard]] temp::TempList *Def() const override;
  [[nodiscard]] temp::TempList *Use() const override;
};

class LabelInstr : public Instr {
public:
  std::string assem_; //  汇编指令文本
  temp::Label *label_;  //  Label的内容
  LabelInstr(std::string assem)
      : assem_(assem), label_(temp::LabelFactory::NamedLabel(assem)) {}

  void Print(FILE *out, temp::Map *m) const override;
  [[nodiscard]] temp::TempList *Def() const override;
  [[nodiscard]] temp::TempList *Use() const override;
};

class MoveInstr : public Instr {
public:
  std::string assem_; //  汇编指令文本
  temp::TempList *dst_, *src_;

  MoveInstr(std::string assem, temp::TempList *dst, temp::TempList *src)
      : assem_(std::move(assem)), dst_(dst), src_(src) {}

  void Print(FILE *out, temp::Map *m) const override;
  [[nodiscard]] temp::TempList *Def() const override;
  [[nodiscard]] temp::TempList *Use() const override;
};

class InstrList {
public:
  InstrList() = default;

  void Print(FILE *out, temp::Map *m) const;
  void Append(assem::Instr *instr) { instr_list_.push_back(instr); }
  void Remove(assem::Instr *instr) { instr_list_.remove(instr); }
  void Insert(std::list<Instr *>::const_iterator pos, assem::Instr *instr) {
    instr_list_.insert(pos, instr);
  }
  [[nodiscard]] const std::list<Instr *> &GetList() const {
    return instr_list_;
  }

private:
  std::list<Instr *> instr_list_;
};

class Proc {
public:
  std::string prolog_;
  InstrList *body_;
  std::string epilog_;

  Proc(std::string prolog, InstrList *body, std::string epilog)
      : prolog_(std::move(prolog)), body_(body), epilog_(std::move(epilog)) {}
};

} // namespace assem

#endif