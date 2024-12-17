#include "tiger/codegen/codegen.h"

#include <cassert>
#include <iostream>
#include <sstream>

extern frame::RegManager *reg_manager;
extern frame::Frags *frags;

namespace {

constexpr int maxlen = 1024;

} // namespace

namespace cg {

void CodeGen::Codegen() {
  temp_map_ = new std::unordered_map<llvm::Value *, temp::Temp *>();  // llvm 对象到临时寄存器的映射
  bb_map_ = new std::unordered_map<llvm::BasicBlock *, int>();
  auto *list = new assem::InstrList();  // 指令列表

  // firstly get all global string's location
  for (auto &&frag : frags->GetList()) {
    if (auto *str_frag = dynamic_cast<frame::StringFrag *>(frag)) {
      auto tmp = temp::TempFactory::NewTemp();  // 分配一个临时寄存器
      list->Append(new assem::OperInstr(
          "leaq " + std::string(str_frag->str_val_->getName()) + "(%rip),`d0",
          new temp::TempList(tmp), new temp::TempList(), nullptr));
      temp_map_->insert({str_frag->str_val_, tmp}); // 字符串llvm对象到临时寄存器的映射
    }
  }

  // move arguments to temp
  auto arg_iter = traces_->GetBody()->arg_begin();
  auto regs = reg_manager->ArgRegs();
  auto tmp_iter = regs->GetList().begin();

  // first arguement is rsp, we need to skip it
  ++arg_iter;
  
  /* 将传入参数全部都移动到新的临时寄存器中 */
  /* 以此来保证用来传参的寄存器可以参与到寄存器分配 */
  for (; arg_iter != traces_->GetBody()->arg_end() &&
         tmp_iter != regs->GetList().end();
       ++arg_iter, ++tmp_iter) {
    auto tmp = temp::TempFactory::NewTemp();
    list->Append(new assem::OperInstr("movq `s0,`d0", new temp::TempList(tmp),
                                      new temp::TempList(*tmp_iter), nullptr));
    temp_map_->insert({static_cast<llvm::Value *>(arg_iter), tmp});
  }

  // pass-by-stack parameters
  /* 传入参数多于可用于传参的寄存器数量*/
  /* 多余参数溢出到栈上 将栈上的传入参数也移动到临时寄存器中*/
  if (arg_iter != traces_->GetBody()->arg_end()) {
    auto last_sp = temp::TempFactory::NewTemp();
    list->Append(
        new assem::OperInstr("movq %rsp,`d0", new temp::TempList(last_sp),
                             new temp::TempList(reg_manager->GetRegister(
                                 frame::X64RegManager::Reg::RSP)),
                             nullptr)); // 栈指针移动到临时寄存器中
    list->Append(new assem::OperInstr(
        "addq $" + std::string(traces_->GetFunctionName()) +
            "_framesize_local,`s0",
        new temp::TempList(last_sp),
        new temp::TempList({last_sp, reg_manager->GetRegister(
                                         frame::X64RegManager::Reg::RSP)}),
        nullptr));  // frame top -> temp reg
    while (arg_iter != traces_->GetBody()->arg_end()) {
      auto tmp = temp::TempFactory::NewTemp();
      list->Append(new assem::OperInstr(
          "movq " +
              std::to_string(8 * (arg_iter - traces_->GetBody()->arg_begin())) +
              "(`s0),`d0",  // param passed by stack -> temp reg
          new temp::TempList(tmp), new temp::TempList(last_sp), nullptr));
      temp_map_->insert({static_cast<llvm::Value *>(arg_iter), tmp});
      ++arg_iter;
    }
  }

  // construct bb_map
  int bb_index = 0;
  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    bb_map_->insert({bb, bb_index++});
  }

  /* 每个basic block中的每条语句返回值映射到临时寄存器 */
  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    // record every return value from llvm instruction
    for (auto &&inst : bb->getInstList())
      temp_map_->insert({&inst, temp::TempFactory::NewTemp()});
  }

  for (auto &&bb : traces_->GetBasicBlockList()->GetList()) {
    // Generate label for basic block
    list->Append(new assem::LabelInstr(std::string(bb->getName())));

    // Generate instructions for basic block
    for (auto &&inst : bb->getInstList())
      InstrSel(list, inst, traces_->GetFunctionName(), bb);
  }

  assem_instr_ = std::make_unique<AssemInstr>(frame::ProcEntryExit2(
      frame::ProcEntryExit1(traces_->GetFunctionName(), list)));
}

void AssemInstr::Print(FILE *out, temp::Map *map) const {
  for (auto instr : instr_list_->GetList())
    instr->Print(out, map);
  fprintf(out, "\n");
}

/**
 * 对单条 llvm 指令进行翻译
 */
void CodeGen::InstrSel(assem::InstrList *instr_list, llvm::Instruction &inst,
                       std::string_view function_name, llvm::BasicBlock *bb) {
  // TODO: your lab5 code here
  // throw std::runtime_error(std::string("Unknown instruction: ") +
  //                          inst.getOpcodeName());
  if (auto *load_instr = llvm::dyn_cast<llvm::LoadInst>(&inst)){
      /* <reg> = load i64, i64* <label>  */
      /* <reg> = load i64, i64* <reg> */
      auto pointer_operand_llvm = load_instr->getPointerOperand();

      if (auto global_variable = llvm::dyn_cast<llvm::GlobalValue>(pointer_operand_llvm)){
          auto global_variable_name = global_variable->getName().str();
          size_t pos = 0;
          if ((pos = global_variable_name.find("_framesize_global", pos))!=std::string::npos){
              global_variable_name.replace(pos, std::string("_framesize_global").length(), "_framesize_local");
          }

          auto dst_reg = this->temp_map_->at(static_cast<llvm::Value *>(&inst));

          auto load_assem = new assem::OperInstr("movq (" + global_variable_name + "),`d0",
                                                 new temp::TempList({dst_reg}),
                                                 nullptr,
                                                 nullptr);
          instr_list->Append(load_assem);
      } else {
          auto pointer_reg = this->temp_map_->at(pointer_operand_llvm);

          auto dst_reg = this->temp_map_->at(static_cast<llvm::Value *>(&inst));

          auto load_assem = new assem::OperInstr("movq (`s0),`d0",
                                                 new temp::TempList({dst_reg}),
                                                 new temp::TempList({pointer_reg}),
                                                 nullptr);
          instr_list->Append(load_assem);
      }

      return;
  }

  if (auto *store_instr = llvm::dyn_cast<llvm::StoreInst>(&inst)) {
      /* store i32 <constant>,i32* <reg> */
      /* store i32 <reg>,i32* <reg> */
      auto pointer_llvm = store_instr->getPointerOperand();
      auto value_llvm = store_instr->getValueOperand();

      temp::Temp *value_temp = nullptr;
      temp::Temp *pointer_temp = nullptr;

      pointer_temp = this->temp_map_->at(pointer_llvm);

      if (auto *constant = llvm::dyn_cast<llvm::ConstantInt>(value_llvm)) {
          auto constant_value = constant->getSExtValue();

          auto store_assem = new assem::OperInstr("movq $" + std::to_string(constant_value) + ",(`s0)",
                                                  nullptr,
                                                  new temp::TempList(pointer_temp),
                                                  nullptr);

          instr_list->Append(store_assem);
      } else {
          value_temp = this->temp_map_->at(value_llvm);
          
          auto store_assem = new assem::OperInstr("movq `s0,(`s1)",
                                                  nullptr,
                                                  new temp::TempList({value_temp,pointer_temp}),
                                                  nullptr);

          instr_list->Append(store_assem);
      }

      return;
  }

  if (auto *calc_instr = llvm::dyn_cast<llvm::BinaryOperator>(&inst)){
      /* <reg> = add/sub/mul/div i32 <reg> <reg> */
      /* <reg> = add/sub/mul/div i32 <reg> <constant> */
      /* <reg> = add/sub/mul/div i32 <constant> <reg> */
      /* 这部分暂时只将 movq <reg>,<reg> 归为 moveInstr,如果有立即数归为operInstr*/
      auto dst_reg = this->temp_map_->at(static_cast<llvm::Value *>(&inst));
      auto l_operand_llvm = calc_instr->getOperand(0);
      auto r_operand_llvm = calc_instr->getOperand(1);

      /* dilemma : EntryExit3 在codeGen之后生成
                    codeGen 中在对每条llvm ir进行翻以前就默认
                    rsp 已经被减为当前函数 frame 对应的 frame 底部
                    所以此处选择对 %rsp = subq %rsp, framesize 不翻译
                    并且将对应语句的生成结果映射为 %rsp */
      if (this->temp_map_->find(l_operand_llvm) == this->temp_map_->end() && 
          calc_instr->getOpcode() == llvm::Instruction::Sub){
            /* xx_sp = sub i64 %0,%xx_local_framesize*/
            /* skip */
            auto reg_map_iter = this->temp_map_->find(static_cast<llvm::Value *>(&inst));
            reg_map_iter->second = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
            
            return;
        }

      auto l_constant_llvm = llvm::dyn_cast<llvm::ConstantInt>(l_operand_llvm);
      auto r_constant_llvm = llvm::dyn_cast<llvm::ConstantInt>(r_operand_llvm);

      auto l_constant_value = l_constant_llvm == nullptr ? (int64_t)0 : l_constant_llvm->getSExtValue();
      auto r_constant_value = r_constant_llvm == nullptr ? (int64_t)0 : r_constant_llvm->getSExtValue();

      auto l_operand_reg = l_constant_llvm == nullptr ? this->temp_map_->at(l_operand_llvm) : nullptr;
      auto r_operand_reg = r_constant_llvm == nullptr ? this->temp_map_->at(r_operand_llvm) : nullptr;

      if (calc_instr->getOpcode() == llvm::Instruction::Add){
          if (l_constant_llvm != nullptr && r_constant_llvm == nullptr){
              auto calc_assem = new assem::OperInstr("leaq " + std::to_string(l_constant_value) + "(`s0),`d0",
                                                     new temp::TempList({dst_reg}),
                                                     new temp::TempList({r_operand_reg}),
                                                     nullptr);
              instr_list->Append(calc_assem);
          } else if (l_constant_llvm == nullptr && r_constant_llvm != nullptr){
              auto calc_assem = new assem::OperInstr("leaq " + std::to_string(r_constant_value) + "(`s0),`d0",
                                                     new temp::TempList({dst_reg}),
                                                     new temp::TempList({l_operand_reg}), 
                                                     nullptr);
              instr_list->Append(calc_assem);
          } else if (l_constant_llvm == nullptr && r_constant_llvm == nullptr){
              auto calc_assem = new assem::OperInstr("leaq (`s0,`s1),`d0",
                                                     new temp::TempList({dst_reg}),
                                                     new temp::TempList({l_operand_reg, r_operand_reg}),
                                                     nullptr);
              instr_list->Append(calc_assem);
          } else {
              instr_list->Append(new assem::OperInstr("movq $" + std::to_string(l_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
              instr_list->Append(new assem::OperInstr("addq $" + std::to_string(r_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
          }
      }

      if (calc_instr->getOpcode() == llvm::Instruction::Sub){
          if (l_constant_llvm != nullptr && r_constant_llvm == nullptr) {
              instr_list->Append(new assem::OperInstr("movq $" + std::to_string(l_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
              instr_list->Append(new assem::OperInstr("subq `s0,`d0",
                                                      new temp::TempList({dst_reg}),
                                                      new temp::TempList({r_operand_reg}),
                                                      nullptr));
          } else if (l_constant_llvm == nullptr && r_constant_llvm != nullptr) {
              instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                      new temp::TempList({dst_reg}),
                                                      new temp::TempList({l_operand_reg})));
              instr_list->Append(new assem::OperInstr("subq $" + std::to_string(l_constant_value)+ ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
          } else if (l_constant_llvm == nullptr && r_constant_llvm == nullptr) {
              instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                      new temp::TempList({dst_reg}),
                                                      new temp::TempList({l_operand_reg})));
              instr_list->Append(new assem::OperInstr("subq `s0,`d0",
                                                      new temp::TempList({dst_reg}),
                                                      new temp::TempList({r_operand_reg}),
                                                      nullptr));
          } else {
              instr_list->Append(new assem::OperInstr("movq $" + std::to_string(l_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
              instr_list->Append(new assem::OperInstr("subq $" + std::to_string(r_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
          }
      }

      if (calc_instr->getOpcode() == llvm::Instruction::Mul){
          if (l_constant_llvm != nullptr && r_constant_llvm == nullptr) {
              instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                      new temp::TempList({dst_reg}),
                                                      new temp::TempList({r_operand_reg})));
              instr_list->Append(new assem::OperInstr("imulq $" + std::to_string(l_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
          } else if (l_constant_llvm == nullptr && r_constant_llvm != nullptr) {
              instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                      new temp::TempList({dst_reg}),
                                                      new temp::TempList({l_operand_reg})));
              instr_list->Append(new assem::OperInstr("imulq $" + std::to_string(r_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
          } else if (l_constant_llvm == nullptr && r_constant_llvm == nullptr) {
              instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                      new temp::TempList({dst_reg}),
                                                      new temp::TempList({l_operand_reg})));
              instr_list->Append(new assem::OperInstr("imulq `s0,`d0",
                                                      new temp::TempList({dst_reg}),
                                                      new temp::TempList({r_operand_reg}),
                                                      nullptr));
          } else {
              instr_list->Append(new assem::OperInstr("movq $" + std::to_string(l_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
              instr_list->Append(new assem::OperInstr("imulq $" + std::to_string(r_constant_value) + ",`d0",
                                                      new temp::TempList({dst_reg}),
                                                      nullptr,
                                                      nullptr));
          }
      }
      
      if (calc_instr->getOpcode() == llvm::Instruction::SDiv){
          auto rax_save_temp = temp::TempFactory::NewTemp();
          auto rdx_save_temp = temp::TempFactory::NewTemp();

          instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                  new temp::TempList({rax_save_temp}),
                                                  new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)})));
          instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                  new temp::TempList({rdx_save_temp}),
                                                  new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RDX)})));
          
          if (l_constant_llvm != nullptr && r_constant_llvm == nullptr) {
              instr_list->Append(new assem::OperInstr("movq $" + std::to_string(l_constant_value) + ",`d0",
                                                      new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)}),
                                                      nullptr,
                                                      nullptr));

              instr_list->Append(new assem::OperInstr("cqto",
                                                      nullptr,
                                                      nullptr,
                                                      nullptr));

              instr_list->Append(new assem::OperInstr("idivq `s0",
                                                      nullptr,
                                                      new temp::TempList({r_operand_reg}),
                                                      nullptr));
          } else if (l_constant_llvm == nullptr && r_constant_llvm != nullptr){
              auto divisor_reg = temp::TempFactory::NewTemp();

              instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                      new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)}),
                                                      new temp::TempList({l_operand_reg})));

              instr_list->Append(new assem::OperInstr("movq $" + std::to_string(r_constant_value) + ",`d0",
                                                      new temp::TempList({divisor_reg}),
                                                      nullptr,
                                                      nullptr));

              instr_list->Append(new assem::OperInstr("cqto",
                                                      nullptr,
                                                      nullptr,
                                                      nullptr));

              instr_list->Append(new assem::OperInstr("idivq `s0",
                                                      nullptr,
                                                      new temp::TempList({divisor_reg}),
                                                      nullptr));
          } else if (l_constant_llvm != nullptr && r_constant_llvm != nullptr) {
              auto divisor_reg = temp::TempFactory::NewTemp();

              instr_list->Append(new assem::OperInstr("movq $" + std::to_string(l_constant_value) + ",`d0",
                                                      new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)}),
                                                      nullptr,
                                                      nullptr));
              instr_list->Append(new assem::OperInstr("movq $" + std::to_string(r_constant_value) + ",`d0",
                                                      new temp::TempList({divisor_reg}),
                                                      nullptr,
                                                      nullptr));
              instr_list->Append(new assem::OperInstr("cqto",
                                                      nullptr,
                                                      nullptr,
                                                      nullptr));

              instr_list->Append(new assem::OperInstr("idivq `s0",
                                                      nullptr,
                                                      new temp::TempList({divisor_reg}),
                                                      nullptr));
          } else if (l_constant_llvm == nullptr && r_constant_llvm == nullptr){
              instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                      new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)}),
                                                      new temp::TempList({l_operand_reg})));
              instr_list->Append(new assem::OperInstr("cqto",
                                                      nullptr,
                                                      nullptr,
                                                      nullptr));
              instr_list->Append(new assem::OperInstr("idivq `s0",
                                                      nullptr,
                                                      new temp::TempList({r_operand_reg}),
                                                      nullptr));
          }

          instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                  new temp::TempList({dst_reg}),
                                                  new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::RAX)})));
          instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                  new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)}),
                                                  new temp::TempList({rax_save_temp})));
          instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                  new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RDX)}),
                                                  new temp::TempList({rdx_save_temp})));
      }

      return;
  }

  if (auto * ptr2int_instr = llvm::dyn_cast<llvm::PtrToIntInst>(&inst)){
    /* <reg> = ptrtoint i32* <reg> to i64 */
    auto dst_reg = this->temp_map_->at(static_cast<llvm::Value *>(&inst));
    auto operand_llvm = ptr2int_instr->getOperand(0);
    auto operand_reg = this->temp_map_->at(operand_llvm);

    instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                            new temp::TempList({dst_reg}),
                                            new temp::TempList({operand_reg})));
    return;
  }

  if (auto * int2ptr_instr = llvm::dyn_cast<llvm::IntToPtrInst>(&inst)){
    /* <reg> = inttoptr i64 <reg> to i32* */
    auto dst_reg = this->temp_map_->at(static_cast<llvm::Value *>(&inst));
    auto operand_llvm = int2ptr_instr->getOperand(0);
    auto operand_reg = this->temp_map_->at(operand_llvm);

    instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                            new temp::TempList({dst_reg}),
                                            new temp::TempList({operand_reg})));
    return;
  }

  if (auto * gep_instr = llvm::dyn_cast<llvm::GetElementPtrInst>(&inst)){
    /* <reg> = getelementptr type,type* <reg>,i32 <constant>/<reg> */
    /* <reg> = getelementptr type,type* <reg>,i32 constant, i32 <constant>/<reg> */
    auto dst_reg = this->temp_map_->at(static_cast<llvm::Value *>(&inst));

    auto pointer_llvm = gep_instr->getOperand(0);

    auto pointer_reg = this->temp_map_->at(pointer_llvm);

    auto index_count = gep_instr->getNumOperands();

    if (index_count == 3){
      /* struct */
      auto index_llvm = gep_instr->getOperand(2);

      if (auto * constant = llvm::dyn_cast<llvm::ConstantInt>(index_llvm)){
          auto constant_value = constant->getSExtValue();
          auto offset = constant_value * 8;

          instr_list->Append(new assem::OperInstr("leaq " + std::to_string(offset) + "(`s0),`d0",
                                                  new temp::TempList({dst_reg}),
                                                  new temp::TempList({pointer_reg}),
                                                  nullptr));
      } else {
          auto index_reg = this->temp_map_->at(index_llvm);

          instr_list->Append(new assem::OperInstr("leaq (`s0,`s1,8),`d0",
                                                  new temp::TempList({dst_reg}),
                                                  new temp::TempList({pointer_reg, index_reg}),
                                                  nullptr));
      }
    } else if (index_count == 4){
      /* array */
      auto index_llvm = gep_instr->getOperand(1);

      if (auto *constant = llvm::dyn_cast<llvm::ConstantInt>(index_llvm)) {
          auto constant_value = constant->getSExtValue();
          auto offset = constant_value * 8;

          instr_list->Append(new assem::OperInstr("leaq " + std::to_string(offset) + "(`s0),`d0",
                                                  new temp::TempList({dst_reg}),
                                                  new temp::TempList({pointer_reg}),
                                                  nullptr));
      } else {
          auto index_reg = this->temp_map_->at(index_llvm);

          instr_list->Append(new assem::OperInstr("leaq (`s0,`s1,8),`d0",
                                                  new temp::TempList({dst_reg}),
                                                  new temp::TempList({pointer_reg, index_reg}),
                                                  nullptr));
      }
    }

    return;
  }

  if (auto * zext_instr = llvm::dyn_cast<llvm::ZExtInst>(&inst)){
    // ?
  }

  if (auto * call_instr = llvm::dyn_cast<llvm::CallInst>(&inst)){
      auto caller_save_regs = reg_manager->CallerSaves();

      std::unordered_map<temp::Temp *, temp::Temp *> temp_caller_reg_map;
      
      /* 保护 caller-save regs */
      for (const auto & caller_reg : caller_save_regs->GetList()){
          auto temp = temp::TempFactory::NewTemp();

          instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                  new temp::TempList({temp}),
                                                  new temp::TempList({caller_reg})));
          temp_caller_reg_map.insert({temp, caller_reg});
      }

      auto arg_operand_count = call_instr->getNumOperands() - 1;

      if (call_instr->getReturnedArgOperand()) {
          arg_operand_count--;
      }

      int arg_operand_index = 0;
      auto arg_regs = reg_manager->ArgRegs();
      auto arg_reg_iter = arg_regs->GetList().begin();

      if (call_instr->getCalledFunction()->isDeclaration()) {
          /* 传参 */
          while (arg_operand_index < arg_operand_count) {
              auto arg_operand_llvm = call_instr->getOperand(arg_operand_index);
              if (arg_reg_iter != arg_regs->GetList().end()) {
                  if (auto *constant = llvm::dyn_cast<llvm::ConstantInt>(arg_operand_llvm)) {
                      auto constant_value = constant->getSExtValue();
                      instr_list->Append(new assem::OperInstr("movq $" + std::to_string(constant_value) + ",`d0",
                                                              new temp::TempList({*arg_reg_iter}),
                                                              nullptr,
                                                              nullptr));
                  } else {
                      auto arg_operand_reg = this->temp_map_->at(arg_operand_llvm);
                      instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                              new temp::TempList({*arg_reg_iter}),
                                                              new temp::TempList({arg_operand_reg})));
                  }
                  arg_reg_iter++;
              } else {
                  if (auto *constant = llvm::dyn_cast<llvm::ConstantInt>(arg_operand_llvm)) {
                      auto constant_value = constant->getSExtValue();
                      instr_list->Append(new assem::OperInstr("movq $" + std::to_string(constant_value) + "," + std::to_string(8 + 8 * arg_operand_index) + "(`s0)",
                                                              nullptr,
                                                              new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)),
                                                              nullptr));
                  } else {
                      auto param_reg = this->temp_map_->at(arg_operand_llvm);
                      instr_list->Append(new assem::OperInstr("movq `s0," + std::to_string(8 + 8 * arg_operand_index) + "(`s1)",
                                                              nullptr,
                                                              new temp::TempList({param_reg, reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                              nullptr));
                  }
              }
              arg_operand_index++;
          }
      } else {
          /* 保存 static link */
          auto static_link_llvm = call_instr->getArgOperand(1);
          auto static_link_reg = this->temp_map_->at(static_link_llvm);

          instr_list->Append(new assem::OperInstr("movq `s0,8(`s1)",
                                                  nullptr,
                                                  new temp::TempList({static_link_reg, reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                  nullptr));

          /* 传参 */
          arg_operand_index = 2;
          while (arg_operand_index < arg_operand_count) {
              auto arg_operand_llvm = call_instr->getOperand(arg_operand_index);
              if (arg_reg_iter != arg_regs->GetList().end()) {
                  if (auto *constant = llvm::dyn_cast<llvm::ConstantInt>(arg_operand_llvm)) {
                      auto constant_value = constant->getSExtValue();
                      instr_list->Append(new assem::OperInstr("movq $" + std::to_string(constant_value) + ",`d0",
                                                              new temp::TempList({*arg_reg_iter}),
                                                              nullptr,
                                                              nullptr));
                  } else {
                      auto arg_operand_reg = this->temp_map_->at(arg_operand_llvm);
                      instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                              new temp::TempList({*arg_reg_iter}),
                                                              new temp::TempList({arg_operand_reg})));
                  }
                  arg_reg_iter++;
              } else {
                  if (auto *constant = llvm::dyn_cast<llvm::ConstantInt>(arg_operand_llvm)) {
                      auto constant_value = constant->getSExtValue();
                      instr_list->Append(new assem::OperInstr("movq $" + std::to_string(constant_value) + "," + std::to_string(8 * arg_operand_index) + "(`s0)",
                                                              nullptr,
                                                              new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)),
                                                              nullptr));
                  } else {
                      auto param_reg = this->temp_map_->at(arg_operand_llvm);
                      instr_list->Append(new assem::OperInstr("movq `s0," + std::to_string(8 * arg_operand_index) + "(`s1)",
                                                              nullptr,
                                                              new temp::TempList({param_reg, reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                              nullptr));
                  }
              }
              arg_operand_index++;
          }
      }

      /* 调用指令 */
      /* FIXME: rsp 需要 +8 ? */
      {
          instr_list->Append(new assem::OperInstr("addq $8,`d0",
                                                  new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                  nullptr,
                                                  nullptr));
          auto callee_function_name = call_instr->getCalledFunction()->getName().str();
          instr_list->Append(new assem::OperInstr("callq " + callee_function_name,
                                                  nullptr,
                                                  nullptr,
                                                  nullptr));
          auto dst_reg = this->temp_map_->at(static_cast<llvm::Value *>(&inst));
          instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                  new temp::TempList({dst_reg}),
                                                  new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RAX)})));

          instr_list->Append(new assem::OperInstr("subq $8,`d0",
                                                  new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                  nullptr,
                                                  nullptr));
      }

      /* 恢复 caller-save regs */
      for (const auto &[temp, caller_reg] : temp_caller_reg_map)
      {
          instr_list->Append(new assem::MoveInstr("movq `s0,`d0",
                                                  new temp::TempList({caller_reg}),
                                                  new temp::TempList({temp})));
      }
  }

  if (auto * ret_instr = llvm::dyn_cast<llvm::ReturnInst>(&inst)){

  }

  if (auto * br_instr = llvm::dyn_cast<llvm::BranchInst>(&inst)){

  }

  if (auto * icmp_instr = llvm::dyn_cast<llvm::ICmpInst>(&inst)){

  }

  if (auto *phi_instr = llvm::dyn_cast<llvm::PHINode>(&inst)){

  }
}

} // namespace cg

/**
 * moveq  - src,dst
 *        - $5, %rax
 *        - %rax,%rax
 *        - (%rax),%rax
 *        - %rax,(%rax)
 * 
 * addq/subq   - src,dst
 *             - $5,%rax
 *             - %rax,%rax
 * imulq  - src,dst
 *        - $5,%rax
 *        - %rax,%rbx
 * 
 *        - src,src,dst
 * 
 * idivq  - src 
 *          被除数 低位 %rax 高位 %rdx
 *          结果   商 %rax 余 %rdx
 * 
 * leaq   src,dst
 * 
 */