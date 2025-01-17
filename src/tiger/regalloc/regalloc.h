#ifndef TIGER_REGALLOC_REGALLOC_H_
#define TIGER_REGALLOC_REGALLOC_H_

#include "tiger/codegen/assem.h"
#include "tiger/codegen/codegen.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/regalloc/color.h"
#include "tiger/util/graph.h"

namespace ra
{

class Result
{
public:
    temp::Map *coloring_;
    assem::InstrList *il_;

    Result() : coloring_(nullptr), il_(nullptr) {}
    Result(temp::Map *coloring, assem::InstrList *il)
        : coloring_(coloring), il_(il) {}
    Result(const Result &result) = delete;
    Result(Result &&result) = delete;
    Result &operator=(const Result &result) = delete;
    Result &operator=(Result &&result) = delete;
    ~Result();
};

class RegAllocator
{
    /* TODO: Put your lab6 code here */
public:
    std::string name_;
    std::unique_ptr<cg::AssemInstr> assem_instr_;
    temp::Map *coloring = nullptr;

    RegAllocator(const std::string &proc_name,
                 std::unique_ptr<cg::AssemInstr> assem_instr) : name_(proc_name), assem_instr_(std::move(assem_instr)) {}
    ~RegAllocator();

    void RegAlloc();

    void Rewrite(const std::vector<live::INodePtr> &spill_vector);

    std::unique_ptr<ra::Result> TransferResult()
    {
        return std::make_unique<ra::Result>(coloring, assem_instr_->GetInstrList());
    }

    void PrintFlowGraph(fg::FGraphPtr flow_graph)
    {
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
};

} // namespace ra

#endif