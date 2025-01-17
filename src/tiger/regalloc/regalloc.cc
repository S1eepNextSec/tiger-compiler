#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"
#include <iostream>

extern frame::RegManager *reg_manager;
extern std::map<std::string, std::pair<int, int>> frame_info_map;

namespace ra
{
/* TODO: Put your lab6 code here */
Result::~Result() {}

RegAllocator::~RegAllocator() {}

void RegAllocator::RegAlloc()
{
    // int count = 0;
    while (true) {
        /* 构建控制流图 */
        auto flow_graph_factory = fg::FlowGraphFactory(this->assem_instr_->GetInstrList());

        flow_graph_factory.AssemFlowGraph();

        auto flow_graph = flow_graph_factory.GetFlowGraph();

        /* 活性分析 */
        auto live_graph_factory = live::LiveGraphFactory(flow_graph);

        live_graph_factory.Liveness();

        auto live_graph = live_graph_factory.GetLiveGraph();

        /* 染色 */
        col::Color color_helper(live_graph);

        auto spill_vector = color_helper.doColor();

        // count++;

        if (spill_vector.size()) {
            Rewrite(spill_vector);
        } else {
            auto color_result = color_helper.DumpColorMapping();

            this->coloring = color_result.coloring;

            break;
        }
    }
}

void RegAllocator::Rewrite(const std::vector<live::INodePtr> &spill_vector)
{
    auto frame_current_size = frame_info_map[this->name_].second;
    frame_current_size = frame_current_size + spill_vector.size() * 8;
    auto frame_current_offset = frame_info_map[this->name_].first;

    for (const auto &spill_node : spill_vector) {
        auto spill_temp = spill_node->NodeInfo();

        frame_current_offset = frame_current_offset - 8;

        std::unordered_set<assem::Instr *> new_inserted_instr;

        auto iter = this->assem_instr_->GetInstrList()->GetList().begin();

        while (iter != this->assem_instr_->GetInstrList()->GetList().end()) {
            if (new_inserted_instr.find(*iter) != new_inserted_instr.end()) {
                iter++;
                continue;
            }

            auto use_temp_list = (*iter)->Use();

            auto def_temp_list = (*iter)->Def();

            /* 使用 */
            if ((use_temp_list && use_temp_list->Contain(spill_temp)) && (def_temp_list == nullptr || !def_temp_list->Contain(spill_temp))) {
                auto replace_temp = temp::TempFactory::NewTemp();

                use_temp_list->Replace(spill_temp, replace_temp);

                auto rsp_add_instr = new assem::OperInstr("leaq " + std::to_string(frame_current_offset) + "(%rsp),%rsp",
                                                          new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                          new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                          nullptr);
                auto load_instr = new assem::OperInstr("movq " + this->name_ + "_framesize_local(%rsp),`d0",
                                                       new temp::TempList({replace_temp}),
                                                       new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                       nullptr);
                auto rsp_sub_instr = new assem::OperInstr("leaq " + std::to_string(-frame_current_offset) + "(%rsp),%rsp",
                                                          new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                          new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                          nullptr);

                new_inserted_instr.insert(rsp_add_instr);
                new_inserted_instr.insert(load_instr);
                new_inserted_instr.insert(rsp_sub_instr);

                this->assem_instr_->GetInstrList()->Insert(iter, rsp_add_instr);
                this->assem_instr_->GetInstrList()->Insert(iter, load_instr);
                this->assem_instr_->GetInstrList()->Insert(iter, rsp_sub_instr);
            }

            /* 定义 */
            if ((use_temp_list == nullptr || !use_temp_list->Contain(spill_temp)) && (def_temp_list && def_temp_list->Contain(spill_temp))) {
                auto replace_temp = temp::TempFactory::NewTemp();

                def_temp_list->Replace(spill_temp, replace_temp);

                auto rsp_add_instr = new assem::OperInstr("leaq " + std::to_string(frame_current_offset) + "(%rsp),%rsp",
                                                          new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                          new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                          nullptr);
                auto store_instr = new assem::OperInstr("movq `s0," + this->name_ + "_framesize_local(%rsp)",
                                                        new temp::TempList({}),
                                                        new temp::TempList({replace_temp, reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                        nullptr);
                auto rsp_sub_instr = new assem::OperInstr("leaq " + std::to_string(-frame_current_offset) + "(%rsp),%rsp",
                                                          new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                          new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                          nullptr);

                new_inserted_instr.insert(rsp_add_instr);
                new_inserted_instr.insert(store_instr);
                new_inserted_instr.insert(rsp_sub_instr);

                this->assem_instr_->GetInstrList()->Insert(std::next(iter), rsp_sub_instr);
                this->assem_instr_->GetInstrList()->Insert(std::next(iter), store_instr);
                this->assem_instr_->GetInstrList()->Insert(std::next(iter), rsp_add_instr);
            }

            /* 使用后又定义 */
            if ((use_temp_list && use_temp_list->Contain(spill_temp)) && (def_temp_list && def_temp_list->Contain(spill_temp))) {
                auto replace_temp = temp::TempFactory::NewTemp();

                use_temp_list->Replace(spill_temp, replace_temp);
                def_temp_list->Replace(spill_temp, replace_temp);

                auto load_rsp_add_instr = new assem::OperInstr("leaq " + std::to_string(frame_current_offset) + "(%rsp),%rsp",
                                                               new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                               new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                               nullptr);

                auto load_instr = new assem::OperInstr("movq " + this->name_ + "_framesize_local(%rsp),`d0",
                                                       new temp::TempList({replace_temp}),
                                                       new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                       nullptr);

                auto load_rsp_sub_instr = new assem::OperInstr("leaq " + std::to_string(-frame_current_offset) + "(%rsp),%rsp",
                                                               new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                               new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                               nullptr);

                auto store_rsp_add_instr = new assem::OperInstr("leaq " + std::to_string(frame_current_offset) + "(%rsp),%rsp",
                                                                new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                                new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                                nullptr);

                auto store_instr = new assem::OperInstr("movq `s0," + this->name_ + "_framesize_local(%rsp)",
                                                        new temp::TempList({}),
                                                        new temp::TempList({replace_temp, reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                        nullptr);

                auto store_rsp_sub_instr = new assem::OperInstr("leaq " + std::to_string(-frame_current_offset) + "(%rsp),%rsp",
                                                                new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                                new temp::TempList({reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)}),
                                                                nullptr);

                new_inserted_instr.insert(load_rsp_add_instr);
                new_inserted_instr.insert(load_rsp_sub_instr);
                new_inserted_instr.insert(load_instr);
                new_inserted_instr.insert(store_instr);
                new_inserted_instr.insert(store_rsp_add_instr);
                new_inserted_instr.insert(store_rsp_sub_instr);

                this->assem_instr_->GetInstrList()->Insert(iter, load_rsp_add_instr);
                this->assem_instr_->GetInstrList()->Insert(iter, load_instr);
                this->assem_instr_->GetInstrList()->Insert(iter, load_rsp_sub_instr);

                this->assem_instr_->GetInstrList()->Insert(std::next(iter), store_rsp_sub_instr);
                this->assem_instr_->GetInstrList()->Insert(std::next(iter), store_instr);
                this->assem_instr_->GetInstrList()->Insert(std::next(iter), store_rsp_add_instr);
            }

            iter++;
        }
    }

    frame_info_map.at(this->name_).first = frame_current_offset;

    frame_info_map.at(this->name_).second = frame_current_size;
}
} // namespace ra