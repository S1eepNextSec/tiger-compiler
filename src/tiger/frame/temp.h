#ifndef TIGER_FRAME_TEMP_H_
#define TIGER_FRAME_TEMP_H_

#include "tiger/symbol/symbol.h"

#include <list>

namespace temp {

using Label = sym::Symbol;

class LabelFactory {
public:
  static Label *NewLabel();
  static Label *NamedLabel(std::string_view name);
  static std::string LabelString(Label *s);

private:
  int label_id_ = 0;
  static LabelFactory label_factory;
};

/**
 * Temp 用来抽象一个临时寄存器
 */
class Temp {
  friend class TempFactory;

public:
  [[nodiscard]] int Int() const;

private:
  int num_; //编号
  explicit Temp(int num) : num_(num) {}
};

/**
 * TempFactory 用来创建一个新的临时寄存器 Temp
 */
class TempFactory {
public:
  static Temp *NewTemp();

private:
  int temp_id_ = 100;
  static TempFactory temp_factory;
};

/**
 * Map 记录临时寄存器到真实寄存器的映射
 */
class Map {
public:
  void Enter(Temp *t, std::string *s);
  std::string *Look(Temp *t);
  void DumpMap(FILE *out);

  static Map *Empty();
  static Map *Name();
  static Map *LayerMap(Map *over, Map *under);

private:
  tab::Table<Temp, std::string> *tab_;
  Map *under_;

  Map() : tab_(new tab::Table<Temp, std::string>()), under_(nullptr) {}
  Map(tab::Table<Temp, std::string> *tab, Map *under)
      : tab_(tab), under_(under) {}
};

class TempList {
public:
  explicit TempList(Temp *t) : temp_list_({t}) {}
  TempList(std::initializer_list<Temp *> list) : temp_list_(list) {}
  TempList() = default;

  void Clear() { temp_list_.clear(); }
  bool Contain(Temp *a) const;
  bool Equal(TempList *tl) const;
  void Replace(Temp *old_temp, Temp *new_temp);
  void Delete(Temp *t);
  void CatList(temp::TempList *tl);
  void Append(Temp *t) { temp_list_.push_back(t); }
  void Prepend(Temp *t) { temp_list_.push_front(t); }
  TempList *Union(temp::TempList *tl) const;
  TempList *Diff(temp::TempList *tl) const;
  [[nodiscard]] Temp *NthTemp(int i) const;
  [[nodiscard]] const std::list<Temp *> &GetList() const { return temp_list_; }

private:
  std::list<Temp *> temp_list_;
};

} // namespace temp

#endif