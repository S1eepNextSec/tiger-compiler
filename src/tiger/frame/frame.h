#ifndef TIGER_FRAME_FRAME_H_
#define TIGER_FRAME_FRAME_H_

#include <list>
#include <memory>
#include <string>

#include "tiger/frame/temp.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

namespace frame {

class RegManager {
public:
  RegManager() : temp_map_(temp::Map::Empty()) {}

  temp::Temp *GetRegister(int regno) { return regs_[regno]; }

  /**
   * Get general-purpose registers except RSI
   * NOTE: returned temp list should be in the order of calling convention
   * @return general-purpose registers
   */
  [[nodiscard]] virtual temp::TempList *Registers() = 0;

  /**
   * Get registers which can be used to hold arguments
   * NOTE: returned temp list must be in the order of calling convention
   * @return argument registers
   */
  [[nodiscard]] virtual temp::TempList *ArgRegs() = 0;

  /**
   * Get caller-saved registers
   * NOTE: returned registers must be in the order of calling convention
   * @return caller-saved registers
   */
  [[nodiscard]] virtual temp::TempList *CallerSaves() = 0;

  /**
   * Get callee-saved registers
   * NOTE: returned registers must be in the order of calling convention
   * @return callee-saved registers
   */
  [[nodiscard]] virtual temp::TempList *CalleeSaves() = 0;

  /**
   * Get return-sink registers
   * @return return-sink registers
   */
  [[nodiscard]] virtual temp::TempList *ReturnSink() = 0;

  /**
   * Get word size
   */
  [[nodiscard]] virtual int WordSize() = 0;

  [[nodiscard]] virtual temp::Temp *FramePointer() = 0;

  temp::Map *temp_map_;

protected:
  std::vector<temp::Temp *> regs_;
};

// 1 Access -> 1 Function param which is used when calling this function
class Access {
public:
  /* TODO: Put your lab5-part1 code here */
    virtual int get_offset() { return 0; }

    virtual ~Access() = default;
};

// 1 Frame -> 1 Function
class Frame {
public:
  int outgo_size_;  // Frame对应的Function 作为Caller 调用 Callee 需要为Callee分配的参数
                    // 在Caller Frame中占用的大小
  int offset_;      // 最后一个local variable位置相对于Frame Top的offset
                    // -offset_ : Frame对应的Function 在其static scope内 需要为local variable分配的空间大小
  temp::Label *name_; // [?] Frame对应的函数名
  std::list<frame::Access *> *formals_; //  调用Frame对应的Function 需要的参数List
                                        //  frame::Access管理每一个参数相对于Frame Top该如何访问到
  llvm::GlobalVariable *framesize_global; // [?]
  llvm::Value *sp;  // [?] Frame Top Address

  Frame(int outgo_size, int offset, temp::Label *name,
        std::list<frame::Access *> *formals)
      : outgo_size_(outgo_size), offset_(offset), name_(name),
        formals_(formals) {}

  [[nodiscard]] virtual std::string GetLabel() const = 0;
  [[nodiscard]] virtual temp::Label *Name() const = 0;
  [[nodiscard]] virtual std::list<frame::Access *> *Formals() const = 0;
  virtual frame::Access *AllocLocal(bool escape) = 0;
  
  /**
   * 为Callee分配传入形参的栈空间
   */
  virtual void AllocOutgoSpace(int size) = 0;
  
  /**
   * @return 计算出的栈中实际栈帧大小
   * 一个函数对应于一个Frame
   * 但一个Frame在实际的栈中会有多个Instances
   * 每一个Instance的Actual Frame Size不一样
   * 因为每个作为Caller调用的Callee不同 Callee 大小不同
   * 简单来说
   * 一个 Fucntion 对应的 Frame
   * 其具体 Instance 的Frame size计算包括
   * 返回地址 + 本地变量空间 + 传出参数空间
   */
  int calculateActualFramesize() {
    return (-offset_ + outgo_size_) + 8;  //  -offset_ : local variables 所占空间
                                          //  outgo_size_:为callee function分配的参数空间
                                          //  8: 返回地址
  }
};

/**
 * Fragments
 */

class Frag {
public:
  virtual ~Frag() = default;

  enum OutputPhase {
    Proc,
    String,
    FrameSize,
  };

  /**
   *Generate assembly for main program
   * @param out FILE object for output assembly file
   */
  virtual void OutputAssem(FILE *out, OutputPhase phase,
                           bool need_ra) const = 0;
};

class StringFrag : public Frag {
public:
  llvm::Value *str_val_;
  std::string str_;

  StringFrag(llvm::Value *str_val, std::string str)
      : str_val_(str_val), str_(std::move(str)) {}

  void OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const override;
};

class FrameSizeFrag : public Frag {
public:
  llvm::Value *framesize_val_;
  int framesize_;

  FrameSizeFrag(llvm::Value *framesize_val, int framesize)
      : framesize_val_(framesize_val), framesize_(framesize) {}

  void OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const override;
};

class ProcFrag : public Frag {
public:
  llvm::Function *body_;

  ProcFrag(llvm::Function *body) : body_(body) {}

  void OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const override;
};

class Frags {
public:
  Frags() = default;
  void PushBack(Frag *frag);
  const std::list<Frag *> &GetList() { return frags_; }

private:
  std::list<Frag *> frags_;
};

frame::Frame *NewFrame(temp::Label *name, std::list<bool> formals);

} // namespace frame

#endif