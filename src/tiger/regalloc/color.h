#ifndef TIGER_COMPILER_COLOR_H
#define TIGER_COMPILER_COLOR_H

#include "tiger/codegen/assem.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/util/graph.h"
#include <unordered_map>
#include <unordered_set>

namespace col
{
struct Result {
    Result() : coloring(nullptr), spills(nullptr) {}
    Result(temp::Map *coloring, live::INodeListPtr spills)
        : coloring(coloring), spills(spills) {}
    temp::Map *coloring;
    live::INodeListPtr spills;
};

class Color
{
    /* TODO: Put your lab6 code here */
public:
    Color(live::LiveGraph live_graph)
    {
        this->graph = live_graph.interf_graph;

        for (auto &pair : live_graph.moves->GetList()) {
            this->move_list.push_back(new std::pair<live::INodePtr, live::INodePtr>(pair.first, pair.second));
        }
    }
    ~Color()
    {
        for (auto p : move_list) {
            delete p;
        }
        move_list.clear();
    }
    std::vector<live::INodePtr> doColor();
    col::Result DumpColorMapping();

private:
    live::IGraphPtr graph;
    std::list<std::pair<live::INodePtr, live::INodePtr> *> move_list;

    int K = 16;

    std::unordered_set<live::INodePtr> machine_regs;    // 记录机器寄存器
    std::unordered_map<live::INodePtr, int> reg_color_map;  // 记录节点到染色编号的映射

    std::unordered_set<live::INodePtr> low_degree_simplify_set; // 低度数 可简化节点
    std::unordered_set<live::INodePtr> low_degree_move_related_set; // 低度数 move相关节点
    std::unordered_set<live::INodePtr> high_degree_set; // 高度数节点
    
    std::unordered_map<live::INodePtr, std::unordered_set<live::INodePtr>> node_interfere_edge_map; // 记录节点的相干关系

    std::unordered_map<live::INodePtr, int> node_current_degree;    // 记录节点的当前度数 因为会在算法执行过程中不断减少度数 但又不希望破坏原始相干关系的记录

    std::unordered_map<live::INodePtr, std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *>> node_move_instr_map;    // 节点到相关的 move 指令集映射

    std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *> unprepared_moves;   // 未准备进行coalesce的move instr
    std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *> potential_moves;    // 有可能进行coalesce的move instr
    std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *> frozen_moves;   // 被冻结 放弃coalesce的move instr
    std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *> constrained_moves;  // 因为存在相干关系而不能coalesce的move instr
    std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *> coalesced_moves;    // coalesce成功的 move instr

    std::unordered_map<live::INodePtr, live::INodePtr> alias_map;   // 记录coalesce关系中哪个寄存器合并到哪个寄存器

    std::unordered_set<live::INodePtr> coalesced_nodes; // 记录被合并了的寄存器
    std::list<live::INodePtr> select_stack; // 存储被简化的节点 最终逐个出栈来染色

    void PreColor();
    void InitNodeAdjsMapping();
    void InitWorkSet();
    void Simplify();
    void DecrementDegree(live::INodePtr inode);
    void AddEdge(live::INodePtr a, live::INodePtr b);
    void AddPotentialMoves(live::INodePtr p);
    void Coalesce();
    void Combine(live::INodePtr target, live::INodePtr source);
    bool BriggsRuleCheck(live::INodePtr a, live::INodePtr b);
    bool GeorgeRuleCheck(live::INodePtr source, live::INodePtr target);
    void Freeze();
    void FreezeRelatedMoves(live::INodePtr p);
    void Spill();
    void AssignColor();
    live::INodePtr SelectNodeToSpill();
    void RewriteCode();

private:
    inline bool isColored(live::INodePtr p)
    {
        return this->reg_color_map.find(p) != this->reg_color_map.end();
    }
    inline bool isPreColored(live::INodePtr p)
    {
        return this->machine_regs.find(p) != this->machine_regs.end();
    }
    inline bool isLowDegree(live::INodePtr p)
    {
        return this->node_current_degree[p] < this->K;
    }

    inline bool isHighDegree(live::INodePtr p)
    {
        return this->node_current_degree[p] >= this->K;
    }

    inline bool isMachineReg(live::INodePtr p)
    {
        return this->machine_regs.find(p) != this->machine_regs.end();
    }

    inline std::vector<std::pair<live::INodePtr, live::INodePtr> *> GetRelatedMoves(live::INodePtr p)
    {
        std::vector<std::pair<live::INodePtr, live::INodePtr> *> result;

        for (const auto &move_pair_p : this->node_move_instr_map[p]) {
            if (this->unprepared_moves.find(move_pair_p) != this->unprepared_moves.end() ||
                this->potential_moves.find(move_pair_p) != this->potential_moves.end()) {
                result.push_back(move_pair_p);
            }
        }

        return result;
    }

    inline std::vector<live::INodePtr> GetCurrentActiveAdjacent(live::INodePtr p)
    {
        std::vector<live::INodePtr> result;

        const auto &set = this->node_interfere_edge_map[p];

        for (const auto &inode : set) {
            if (std::find(this->select_stack.begin(), this->select_stack.end(), inode) != this->select_stack.end()) {
                continue;
            }

            if (this->coalesced_nodes.find(inode) != this->coalesced_nodes.end()) {
                continue;
            }

            result.push_back(inode);
        }

        return result;
    }

    inline bool hasInterfereEdge(live::INodePtr a, live::INodePtr b)
    {
        if (this->isMachineReg(a) && this->isMachineReg(b)) {
            return true;
        }

        if (!this->isMachineReg(a)) {
            return this->node_interfere_edge_map[a].find(b) != this->node_interfere_edge_map[a].end();
        }

        if (!this->isMachineReg(b)) {
            return this->node_interfere_edge_map[b].find(a) != this->node_interfere_edge_map[b].end();
        }

        return false;
    }

    inline bool isMoveRelated(live::INodePtr p)
    {
        return this->GetRelatedMoves(p).size() != 0;
    }

    inline live::INodePtr GetAlias(live::INodePtr p)
    {
        auto iter = this->alias_map.find(p);

        while (iter != this->alias_map.end()) {
            p = iter->second;
            iter = this->alias_map.find(p);
        }

        return p;
    }
};
} // namespace col

#endif // TIGER_COMPILER_COLOR_H
