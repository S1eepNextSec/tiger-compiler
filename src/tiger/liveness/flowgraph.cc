#include "tiger/liveness/flowgraph.h"
#include <iostream>
#include <unordered_set>
namespace fg
{

void FlowGraphFactory::AssemFlowGraph()
{
    /* TODO: Put your lab6 code here */
    fg::FNodePtr last_node = nullptr;

    /* 主要思路是先对每一个basic block做指令节点的连接 然后再进行basic block之间的控制流连接*/
    for (const auto &instr : this->instr_list_->GetList()) {
        auto new_flow_graph_node = this->flowgraph_->NewNode(instr);

        if (last_node) {
            this->flowgraph_->AddEdge(last_node, new_flow_graph_node);
        }

        last_node = new_flow_graph_node;

        if (typeid(*instr) == typeid(assem::LabelInstr)) {
            auto label_instr = static_cast<assem::LabelInstr *>(instr);

            auto label_name = label_instr->label_->Name();

            /* 将 Label 节点暂存,等待所有BasicBlock翻译完成再连接控制流 */
            this->label_map_->insert({label_name, new_flow_graph_node});

            continue;
        }

        /* fall through 情况会直接处理掉*/

        /* 但遇到 jmp 和 ret 需要特别处理,保证它们的控制流不会指向连续的下一条指令 */

        if (typeid(*instr) == typeid(assem::OperInstr)) {
            auto oper_instr = static_cast<assem::OperInstr *>(instr);

            if (oper_instr->assem_.find("jmp") != std::string::npos) {
                /* 是 jmp 指令 */
                last_node = nullptr;
            }

            if (oper_instr->assem_.find("ret") != std::string::npos) {
                /* 是 ret 指令 */
                last_node = nullptr;
            }
        }
    }

    std::unordered_set<std::string> handled_label;

    /* basic block 之间做连接 */
    for (const auto &[basic_block_label, node] : *(this->label_map_)) {
        if (handled_label.find(basic_block_label) != handled_label.end()) {
            continue;
        }

        handled_label.insert(basic_block_label);

        auto current_node = node;

        while (current_node) {
            auto succ_list = current_node->Succ();

            fg::FNodePtr next_node = nullptr;

            if (succ_list->GetList().size() != 0) {
                /* 第一项永远是该BasicBlock中的下一条语句 */
                next_node = *succ_list->GetList().begin();
            }

            auto current_instr = current_node->NodeInfo();

            if (typeid(*current_instr) == typeid(assem::OperInstr)) {
                auto oper_instr = static_cast<assem::OperInstr *>(current_instr);

                if (oper_instr->jumps_) {
                    auto target_label = oper_instr->jumps_->labels_->at(0);

                    auto target_node = this->label_map_->at(target_label->Name());

                    this->flowgraph_->AddEdge(current_node, target_node);
                }
            }

            if (typeid(*current_instr) == typeid(assem::LabelInstr)) {
                /* fall through case */
                auto label_instr = static_cast<assem::LabelInstr *>(current_instr);

                handled_label.insert(label_instr->assem_);
            }

            current_node = next_node;
        }
    }
}

} // namespace fg

namespace assem
{

temp::TempList *LabelInstr::Def() const { return new temp::TempList(); }

temp::TempList *MoveInstr::Def() const { return dst_; }

temp::TempList *OperInstr::Def() const { return dst_; }

temp::TempList *LabelInstr::Use() const { return new temp::TempList(); }

temp::TempList *MoveInstr::Use() const
{
    return (src_) ? src_ : new temp::TempList();
}

temp::TempList *OperInstr::Use() const
{
    return (src_) ? src_ : new temp::TempList();
}
} // namespace assem
