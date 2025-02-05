#include "tiger/frame/x64frame.h"
#include "tiger/env/env.h"

#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

extern frame::RegManager *reg_manager;
extern llvm::IRBuilder<> *ir_builder;
extern llvm::Module *ir_module;

namespace frame {

X64RegManager::X64RegManager() : RegManager() {
  for (int i = 0; i < REG_COUNT; i++)
    regs_.push_back(temp::TempFactory::NewTemp());

  // Note: no frame pointer in tiger compiler
  std::array<std::string_view, REG_COUNT> reg_name{
      "%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%rbp", "%rsp",
      "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"};
  int reg = RAX;
  for (auto &name : reg_name) {
    temp_map_->Enter(regs_[reg], new std::string(name));
    reg++;
  }
}

/**
 * @return 返回通用寄存器(rsp 除外)
 */
temp::TempList *X64RegManager::Registers() {
  const std::array reg_array{
      RAX, RBX, RCX, RDX, RSI, RDI, RBP, R8, R9, R10, R11, R12, R13, R14, R15,
  };
  auto *temp_list = new temp::TempList();
  for (auto &reg : reg_array)
    temp_list->Append(regs_[reg]);
  return temp_list;
}

/**
 * @return 返回存储函数调用传入参数的寄存器
 */
temp::TempList *X64RegManager::ArgRegs() {
  const std::array reg_array{RDI, RSI, RDX, RCX, R8, R9};
  auto *temp_list = new temp::TempList();
  ;
  for (auto &reg : reg_array)
    temp_list->Append(regs_[reg]);
  return temp_list;
}

/**
 * @return 返回 Caller save 寄存器
 */
temp::TempList *X64RegManager::CallerSaves() {
  std::array reg_array{RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11};
  auto *temp_list = new temp::TempList();
  ;
  for (auto &reg : reg_array)
    temp_list->Append(regs_[reg]);
  return temp_list;
}

/**
 * @return 返回 Callee save 寄存器
 */
temp::TempList *X64RegManager::CalleeSaves() {
  std::array reg_array{RBP, RBX, R12, R13, R14, R15};
  auto *temp_list = new temp::TempList();
  ;
  for (auto &reg : reg_array)
    temp_list->Append(regs_[reg]);
  return temp_list;
}

/**
 * @return 返回函数调用结束后 live out 的寄存器
 */
temp::TempList *X64RegManager::ReturnSink() {
  temp::TempList *temp_list = CalleeSaves();
  temp_list->Append(regs_[SP]);
  temp_list->Append(regs_[RV]);
  return temp_list;
}

int X64RegManager::WordSize() { return 8; }

/**
 * @return 获取栈指针寄存器
 */
temp::Temp *X64RegManager::FramePointer() { return regs_[FP]; }

/**
 * x86 Frame中每一个变量的具体访问管理
 * 包括本地变量以及传入的参数
 */
class InFrameAccess : public Access {
public:
  int offset; // 相对于stack frame top 的 offset
  frame::Frame *parent_frame; // 记录函数的 frame 结构

  explicit InFrameAccess(int offset, frame::Frame *parent)
      : offset(offset), parent_frame(parent) {}

  /* TODO: Put your lab5-part1 code here */
  int get_offset() override { return offset; }
};

// inherited from Frame Class
// Frame 定义独立于各个平台具体的栈管理形式
// 继承 Frame 来实现具体平台中的栈帧管理
class X64Frame : public Frame {
public:
  X64Frame(temp::Label *name, std::list<frame::Access *> *formals)
      : Frame(8, 0, name, formals) {}

  [[nodiscard]] std::string GetLabel() const override { return name_->Name(); }
  [[nodiscard]] temp::Label *Name() const override { return name_; }
  [[nodiscard]] std::list<frame::Access *> *Formals() const override {
    return formals_;
  }

  /**
   * 在栈帧中分配一个本地变量
   */
  frame::Access *AllocLocal(bool escape) override {
    frame::Access *access;

    offset_ -= reg_manager->WordSize();
    access = new InFrameAccess(offset_, this);  //  构造InFrameAccess来记录对本地变量的访问方式

    return access;
  }

  /**
   * 为Callee在Caller Frame中分配一段空间用来传递参数
   */
  void AllocOutgoSpace(int size) override {
    if (size > outgo_size_)
      outgo_size_ = size;
  }
};

/**
 * 为一个 Function 构造一个 Frame 来记录其在栈中的存储形式
 * @param name: 函数名
 * @param formals: 形参逃逸列表 true表示应该在栈中存储 false表示应在寄存器中存储
 * @return 构造出的栈帧
 */
frame::Frame *NewFrame(temp::Label *name, std::list<bool> formals) {
  /* TODO: Put your lab5-part1 code here */
  // formals 不包含 static link
  auto x64_frame = new frame::X64Frame(name, nullptr);

  std::list<frame::Access *> *formals_list = new std::list<frame::Access *>;
  x64_frame->formals_ = formals_list;

  int offset = 8;

  // static link
  formals_list->push_back(new frame::InFrameAccess(offset, x64_frame));

  offset += 8;

  for (const auto & is_escape : formals){
    if (is_escape || ! is_escape){  // 不管是否 escape 都先栈上存储
        auto in_frame_access = new frame::InFrameAccess(offset, x64_frame);
        offset += 8;

        formals_list->push_back(in_frame_access);
    }
  }

  // Global Variable Frame Size unset
  // stack pointer unset

  auto global_frame_size = (llvm::GlobalVariable *)ir_module->getOrInsertGlobal(name->Name() + "_framesize_global",
                                                                                llvm::Type::getInt64Ty(ir_module->getContext()));

  x64_frame->framesize_global = global_frame_size;

  // global_frame_size value not decided yet
  // stack pointer unset yet
  

  return x64_frame;
}

/**
 * Moving incoming formal parameters, the saving and restoring of callee-save
 * Registers
 * @param frame curruent frame
 * @param stm statements
 * @return statements with saving, restoring and view shift
 */
assem::InstrList *ProcEntryExit1(std::string_view function_name,
                                 assem::InstrList *body) {
  // TODO: your lab5 code here

  auto callee_save_regs = reg_manager->CalleeSaves();
  
  body->Append(new assem::LabelInstr(std::string(function_name) + "_ret"));

  // 把传入的参数全部都从机器寄存器移动到无限的临时寄存器
  // 保证所有机器寄存器一开始都可以参与到寄存器分配
  // 同时函数末尾添加将对应临时寄存器中保存的值移回机器寄存器的指令
  for (const auto & reg : callee_save_regs->GetList()){
      auto temp = temp::TempFactory::NewTemp();

      body->PushFront(new assem::MoveInstr("movq `s0,`d0",
                                           new temp::TempList({temp}),
                                           new temp::TempList({reg})));
      body->Append(new assem::MoveInstr("movq `s0,`d0",
                                        new temp::TempList({reg}),
                                        new temp::TempList({temp})));
  }

  return body;
}

/**
 * Appends a “sink” instruction to the function body to tell the register
 * allocator that certain registers are live at procedure exit
 * @param body function body
 * @return instructions with sink instruction
 */
assem::InstrList *ProcEntryExit2(assem::InstrList *body) {
  // 加一条空指令 在 liveness 分析的时候可以保证函数出口处机器寄存器存活
  body->Append(new assem::OperInstr("", new temp::TempList(),
                                    reg_manager->ReturnSink(), nullptr));
  return body;
}

/**
 * The procedure entry/exit sequences
 * @param frame the frame of current func
 * @param body current function body
 * @return whole instruction list with prolog_ end epilog_
 */
assem::Proc *ProcEntryExit3(std::string_view function_name,
                            assem::InstrList *body) {
  std::string prologue = "";
  std::string epilogue = "";

  // 调整栈指针
  body->PushFront(new assem::OperInstr("subq $" + std::string(function_name) + "_framesize_local,`d0",
                                       new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                       nullptr,
                                       nullptr));

  body->PushFront(new assem::LabelInstr(std::string(function_name)));

  // 末尾“退栈”
  body->Append(new assem::OperInstr("addq $" + std::string(function_name) + "_framesize_local,`d0",
                                    new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)),
                                    nullptr,
                                    nullptr));
  // 返回
  body->Append(new assem::OperInstr("retq",
                                    nullptr,
                                    nullptr,
                                    nullptr));

  // TODO: your lab5 code here
  return new assem::Proc(prologue, body, epilogue);
}

void Frags::PushBack(Frag *frag) { frags_.emplace_back(frag); }

} // namespace frame