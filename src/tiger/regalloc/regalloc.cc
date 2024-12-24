#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"
#include <iostream>

extern frame::RegManager *reg_manager;
extern std::map<std::string, std::pair<int, int>> frame_info_map;

namespace ra {
/* TODO: Put your lab6 code here */
    Result::~Result(){}

    RegAllocator::~RegAllocator(){}

    void RegAllocator::RegAlloc(){
        auto flow_graph_factory = fg::FlowGraphFactory(this->assem_instr_->GetInstrList());

        flow_graph_factory.AssemFlowGraph();

        auto flow_graph = flow_graph_factory.GetFlowGraph();

        bool to_print_flow_graph = false;

        if (to_print_flow_graph){
            /* print flow graph */
            FILE *file = fopen("/home/stu/tiger-compiler/flow_graph_out_put.txt", "w");

            for (const auto &node : flow_graph->Nodes()->GetList()) {
                fprintf(file, "%d ", node->Key());

                auto succ = node->Succ();

                for (const auto &succ_ : succ->GetList()) {
                    fprintf(file, "%d ", succ_->Key());
                }

                fprintf(file, "\n");
            }

            flow_graph->Show(file, flow_graph->Nodes(), [&](assem::Instr *i) {
                if (typeid(*i) == typeid(assem::OperInstr)) {
                    fprintf(file, "%s", static_cast<assem::OperInstr *>(i)->assem_.c_str());
                }
                if (typeid(*i) == typeid(assem::MoveInstr)) {
                    fprintf(file, "%s", static_cast<assem::MoveInstr *>(i)->assem_.c_str());
                }
                if (typeid(*i) == typeid(assem::LabelInstr)) {
                    fprintf(file, "%s", static_cast<assem::LabelInstr *>(i)->assem_.c_str());
                }
            });
            fflush(file);
            /* print flow graph */
        }

        auto live_graph_factory = live::LiveGraphFactory(flow_graph);

        live_graph_factory.Liveness();
    }
} // namespace ra