#include "tiger/liveness/liveness.h"
#include <queue>
#include <unordered_set>
extern frame::RegManager *reg_manager;

namespace live {

bool MoveList::Contain(INodePtr src, INodePtr dst) {
  return std::any_of(move_list_.cbegin(), move_list_.cend(),
                     [src, dst](std::pair<INodePtr, INodePtr> move) {
                       return move.first == src && move.second == dst;
                     });
}

void MoveList::Delete(INodePtr src, INodePtr dst) {
  assert(src && dst);
  auto move_it = move_list_.begin();
  for (; move_it != move_list_.end(); move_it++) {
    if (move_it->first == src && move_it->second == dst) {
      break;
    }
  }
  move_list_.erase(move_it);
}

MoveList *MoveList::Union(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : move_list_) {
    res->move_list_.push_back(move);
  }
  for (auto move : list->GetList()) {
    if (!res->Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

MoveList *MoveList::Intersect(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : list->GetList()) {
    if (Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

void LiveGraphFactory::LiveMap() {
  /* TODO: Put your lab6 code here */

  /* 找到出度为 0 的节点,必定是一个函数的结束 */
  auto flow_node_list_p = this->flowgraph_->Nodes();

  for (auto flow_iter = flow_node_list_p->GetList().begin(); flow_iter != flow_node_list_p->GetList().end();flow_iter++){
      this->in_->Enter(*flow_iter, new temp::TempList({}));
      this->out_->Enter(*flow_iter, new temp::TempList({}));
  }

  while (true){
      bool changed = false;

      for (auto flow_iter = flow_node_list_p->GetList().rbegin(); flow_iter != flow_node_list_p->GetList().rend(); flow_iter++) {
          auto use_list = (*flow_iter)->NodeInfo()->Use();
          auto def_list = (*flow_iter)->NodeInfo()->Def();

          auto cur_out = this->out_->Look(*flow_iter);
          auto cur_in = this->in_->Look(*flow_iter);

          bool cur_in_changed = false;
          bool cur_out_changed = false;

          {
            /* update out */
            auto succ_list = (*flow_iter)->Succ();

            temp::TempList tmp_list;

            for (const auto & succ : succ_list->GetList()){
                auto succ_in = this->in_->Look(succ);

                for (const auto & succ_in_temp : succ_in->GetList()){
                    if (!tmp_list.Contain(succ_in_temp)){
                        tmp_list.Append(succ_in_temp);
                    }
                }
            }

            if (tmp_list.GetList().size() != cur_out->GetList().size()){
                cur_out_changed = true;
            } else {
                for (const auto &temp : tmp_list.GetList()) {
                    if (!cur_out->Contain(temp)) {
                        cur_out_changed = true;
                        break;
                    }
                }
            }

            if (cur_out_changed){
                cur_out->Clear();

                for (const auto &temp: tmp_list.GetList()){
                    cur_out->Append(temp);
                }
            }

          }

          {
            /* update in */
            auto tmp_list = *cur_out;

            if (def_list) {
                for (const auto &temp : def_list->GetList()) {
                    tmp_list.Delete(temp);
                }
            }

            if (use_list) {
                for (const auto &temp : use_list->GetList()) {
                    if (!tmp_list.Contain(temp)) {
                        tmp_list.Append(temp);
                    }
                }
            }

            if (tmp_list.GetList().size() != cur_in->GetList().size()) {
                cur_in_changed = true;
            } else {
                for (const auto &temp : tmp_list.GetList()) {
                    if (!cur_in->Contain(temp)) {
                        cur_in_changed = true;
                        break;
                    }
                }
            }

            if (cur_in_changed){
                cur_in->Clear();

                for (const auto &temp : tmp_list.GetList()) {
                    cur_in->Append(temp);
                }
            }
          }

          changed = changed || cur_in_changed || cur_out_changed;
      }

      if (!changed){
          break;
      }
  }
}

void LiveGraphFactory::InterfGraph() {
  /* TODO: Put your lab6 code here */
  for (const auto & fnode : this->flowgraph_->Nodes()->GetList()){
      auto instr = fnode->NodeInfo();

      auto out_set = *this->out_->Look(fnode);

      auto use_p = fnode->NodeInfo()->Use();
      auto def_p = fnode->NodeInfo()->Def();

      auto use_reg_list = use_p ? *use_p : temp::TempList();
      auto def_reg_list = def_p ? *def_p : temp::TempList();

      if (typeid(*instr) == typeid(assem::MoveInstr)) {
          for (const auto &use_reg : use_reg_list.GetList()) {
              out_set.Delete(use_reg);
          }
      }

      for (const auto & def_reg : def_reg_list.GetList()){
          if (out_set.Contain(def_reg)) {
              continue;
          }

          out_set.Append(def_reg);
      }

      for (const auto & def_reg : def_reg_list.GetList()){
        for (const auto & target : out_set.GetList()){
            if (def_reg == target){
                continue;
            }

            live::INodePtr node_i = this->temp_node_map_->Look(def_reg);
            live::INodePtr node_j = this->temp_node_map_->Look(target);

            if (!node_i) {
                node_i = this->live_graph_.interf_graph->NewNode(def_reg);
                this->temp_node_map_->Enter(def_reg, node_i);
            }

            if (!node_j) {
                node_j = this->live_graph_.interf_graph->NewNode(target);
                this->temp_node_map_->Enter(target, node_j);
            }

            if (!node_i->Adj(node_j)) {
                this->live_graph_.interf_graph->AddEdge(node_i, node_j);
            }
        }
      }
      //   auto temp_count = out_set->GetList().size();

      //   for (int i = 0; i < temp_count; i++){
      //       for (int j = i + 1; j < temp_count;j++){
      //           auto temp_i = out_set->NthTemp(i);
      //           auto temp_j = out_set->NthTemp(j);

      //           live::INodePtr node_i = this->temp_node_map_->Look(temp_i);
      //           live::INodePtr node_j = this->temp_node_map_->Look(temp_j);

      //           if (! node_i){
      //               node_i = this->live_graph_.interf_graph->NewNode(temp_i);
      //               this->temp_node_map_->Enter(temp_i, node_i);
      //           }

      //           if (! node_j){
      //               node_j = this->live_graph_.interf_graph->NewNode(temp_j);
      //               this->temp_node_map_->Enter(temp_j, node_j);
      //           }

      //           if (! node_i->Adj(node_j)){
      //               this->live_graph_.interf_graph->AddEdge(node_i, node_j);
      //           }
      //       }
      //   }

      if (typeid(*instr) == typeid(assem::MoveInstr)){
          auto use_reg = *instr->Use()->GetList().begin();
          auto def_reg = *instr->Def()->GetList().begin();

          auto node_use = this->temp_node_map_->Look(use_reg);
          auto node_def = this->temp_node_map_->Look(def_reg);

          if (!node_use) {
              node_use = this->live_graph_.interf_graph->NewNode(use_reg);
              this->temp_node_map_->Enter(use_reg, node_use);
          }

          if (!node_def) {
              node_def = this->live_graph_.interf_graph->NewNode(def_reg);
              this->temp_node_map_->Enter(def_reg, node_def);
          }

          if (!this->live_graph_.moves->Contain(node_use, node_def)) {
              this->live_graph_.moves->Append(node_use, node_def);
          }
      }

      std::unordered_set<live::INodePtr> check;
  }
}

void LiveGraphFactory::Liveness() {
  LiveMap();

//   this->PrintLiveOutSet(reg_manager);
  
  InterfGraph();
}

} // namespace live