#ifndef TIGER_COMPILER_COLOR_H
#define TIGER_COMPILER_COLOR_H

#include "tiger/codegen/assem.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/util/graph.h"
#include <unordered_set>
#include <unordered_map>

namespace col {
struct Result {
  Result() : coloring(nullptr), spills(nullptr) {}
  Result(temp::Map *coloring, live::INodeListPtr spills)
      : coloring(coloring), spills(spills) {}
  temp::Map *coloring;
  live::INodeListPtr spills;
};

class Color {
  /* TODO: Put your lab6 code here */
public:
  Color(live::LiveGraph live_graph){
      this->graph = live_graph.interf_graph;

      for (auto &pair : live_graph.moves->GetList()) {
          this->move_list.push_back(new std::pair<live::INodePtr, live::INodePtr>(pair.first, pair.second));
      }
  }
  ~Color(){
    for (auto p : move_list){
        delete p;
    }
    move_list.clear();
  }
  std::vector<live::INodePtr> doColor();
  col::Result DumpColorMapping();

private:
  live::IGraphPtr graph;
  std::list<std::pair<live::INodePtr, live::INodePtr>*> move_list;

  int K = 16;

  std::unordered_set<live::INodePtr> machine_regs;
  std::unordered_map<live::INodePtr, int> reg_color_map;

  std::unordered_set<live::INodePtr> low_degree_simplify_set;
  std::unordered_set<live::INodePtr> low_degree_move_related_set;
  std::unordered_set<live::INodePtr> high_degree_set;

  std::unordered_map<live::INodePtr,std::unordered_set<live::INodePtr>> node_adj_map;

//   std::multimap<live::INodePtr, std::pair<live::INodePtr, live::INodePtr> *> node_move_instr_map;
  std::unordered_map<live::INodePtr, std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *>> node_move_instr_map;

  std::unordered_set<std::pair<live::INodePtr, live::INodePtr>*> unprepared_moves;
  std::unordered_set<std::pair<live::INodePtr, live::INodePtr>*> potential_moves;
  std::unordered_set<std::pair<live::INodePtr, live::INodePtr>*> frozen_moves;
  std::unordered_set<std::pair<live::INodePtr, live::INodePtr>*> constrained_moves;
  std::unordered_set<std::pair<live::INodePtr, live::INodePtr>*> coalesced_moves;

  std::unordered_map<live::INodePtr, live::INodePtr> alias_map;

  std::unordered_set<live::INodePtr> coalesced_nodes;
  std::list<live::INodePtr> select_stack;

  void PreColor();
  void InitNodeAdjsMapping();
  void InitWorkSet();
  void Simplify();
  void DeleteEdge(live::INodePtr src,live::INodePtr adj);
  void AddPotentialMoves(live::INodePtr p);
  void Coalesce();
  void Combine(live::INodePtr target,live::INodePtr source);
  bool BriggsRuleCheck(live::INodePtr a, live::INodePtr b);
  bool GeorgeRuleCheck(live::INodePtr source, live::INodePtr target);
  void Freeze();
  void FreezeRelatedMoves(live::INodePtr p);
  void Spill();
  void AssignColor();
  live::INodePtr SelectNodeToSpill();
  void RewriteCode();

  private:
  inline bool isAdj(live::INodePtr p,live::INodePtr q){
    if (this->isMachineReg(p) && this->isMachineReg(q)){
        return true;
    }

    if (this->isMachineReg(p)){
        if(this->node_adj_map[q].find(p) != this->node_adj_map[q].end()){
            return true;
        }
        return false;
    }

    if (this->isMachineReg(q)){
        if (this->node_adj_map[p].find(q) != this->node_adj_map[p].end()){
            return true;
        }
        return false;
    }

    if (this->node_adj_map[p].find(q) != this->node_adj_map[p].end() ||
        this->node_adj_map[q].find(p) != this->node_adj_map[q].end()){
        return true;
    }
    
    return false;
  }
  inline bool isColored(live::INodePtr p){
      return this->reg_color_map.find(p) != this->reg_color_map.end();
  }
  inline bool isPreColored(live::INodePtr p){
      return this->machine_regs.find(p) != this->machine_regs.end();
  }
  inline bool isLowDegree(live::INodePtr p){
      return this->GetDegree(p) < K;
  }

  inline bool isHighDegree(live::INodePtr p){
      return this->GetDegree(p) >= K;
  }

  inline bool isMachineReg(live::INodePtr p){
      return this->machine_regs.find(p) != this->machine_regs.end();
  }

  inline std::vector<std::pair<live::INodePtr, live::INodePtr> *> getRelatedMoves(live::INodePtr p)
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

  inline std::vector<live::INodePtr> getAdjacent(live::INodePtr p){
      std::vector<live::INodePtr> result;

      auto set = this->node_adj_map[p];

      for (const auto & inode : set){
          result.push_back(inode);
      }

      return result;
  }

  inline bool isMoveRelated(live::INodePtr p){
      return this->getRelatedMoves(p).size() != 0;
  }

  inline live::INodePtr GetAlias(live::INodePtr p){
      auto iter = this->alias_map.find(p);

      while (iter != this->alias_map.end()){
          p = iter->second;
          iter = this->alias_map.find(p);
      }

      return p;
  }

  inline int GetDegree(live::INodePtr p){
    if (this->machine_regs.find(p) != this->machine_regs.end()){
        return this->K;
    } else {
        return this->node_adj_map[p].size();
    }
  }
};
} // namespace col

#endif // TIGER_COMPILER_COLOR_H
