#ifndef TIGER_TRANSLATE_TRANSLATE_H_
#define TIGER_TRANSLATE_TRANSLATE_H_

#include <list>
#include <memory>

#include "tiger/absyn/absyn.h"
#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/semant/types.h"

namespace tr {

class Exp;
class ValAndTy;
class Level;

/**
 * Frame中对变量的访问管理
 * Level* 记录变量所在的层级 辅助 static link计算
 * frame::Access* 记录对frame中变量的访问管理 
 */
class Access {
public:
  Level *level_;
  frame::Access *access_;

  Access(Level *level, frame::Access *access)
      : level_(level), access_(access) {}
  static Access *AllocLocal(Level *level, bool escape);
};

/**
 * Level用以记录 Function 对应的 Frame 在 源码 static scope 中的函数嵌套层次
 * 层次信息辅助判断 static link 
 * Level* parent_ 指向该函数所在的父函数(该函数在父函数中定义)
 * frame::Frame * frame_ 为对应的函数 Frame
 */
class Level {
public:
  frame::Frame *frame_;
  Level *parent_;
  // llvm::Value *sp;

  Level(frame::Frame *frame, Level *parent) : frame_(frame), parent_(parent) {}

  llvm::Value *get_sp() { return frame_->sp; }
  
  void set_sp(llvm::Value *sp) { frame_->sp = sp; }

  /* TODO: Put your lab5-part1 code here */
};

class ProgTr {
public:
  ProgTr(std::unique_ptr<absyn::AbsynTree> absyn_tree,
         std::unique_ptr<err::ErrorMsg> errormsg)
      : absyn_tree_(std::move(absyn_tree)), errormsg_(std::move(errormsg)),
        main_level_(std::make_unique<Level>(
            frame::NewFrame(temp::LabelFactory::NamedLabel("tigermain"),
                            std::list<bool>()),
            nullptr)),
        tenv_(std::make_unique<env::TEnv>()),
        venv_(std::make_unique<env::VEnv>()) {}

  /**
   * Translate IR tree
   */
  void Translate();

  /**
   * Transfer the ownership of errormsg to outer scope
   * @return unique pointer to errormsg
   */
  std::unique_ptr<err::ErrorMsg> TransferErrormsg() {
    return std::move(errormsg_);
  }

  void OutputIR(std::string_view filename);

private:
  std::unique_ptr<absyn::AbsynTree> absyn_tree_;  // AST
  std::unique_ptr<err::ErrorMsg> errormsg_;
  std::unique_ptr<Level> main_level_; //  main函数 Level信息 / Level树入口
  std::unique_ptr<env::TEnv> tenv_; //type environment
  std::unique_ptr<env::VEnv> venv_; //variable & function environment

  // Fill base symbol for var env and type env
  void FillBaseVEnv();
  void FillBaseTEnv();
};

} // namespace tr

#endif
