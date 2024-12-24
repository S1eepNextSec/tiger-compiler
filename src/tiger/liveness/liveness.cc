#include "tiger/liveness/liveness.h"
#include <queue>
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

}

void LiveGraphFactory::Liveness() {
  LiveMap();

  bool to_print_liveness = false;

  if (to_print_liveness){
      {
          /* print liveness */
          FILE *live_file = fopen("/home/stu/tiger-compiler/live_output.txt", "w");

          for (const auto &node : this->flowgraph_->Nodes()->GetList()) {
              auto in_ = this->in_->Look(node);
              auto out_ = this->out_->Look(node);

              fprintf(live_file, "{");

              if (in_) {
                  for (const auto &temp : in_->GetList()) {
                      auto name = reg_manager->temp_map_->Look(temp);

                      if (name) {
                          fprintf(live_file, "%s ", name->c_str());
                      } else {
                          fprintf(live_file, "%s ", ("t" + std::to_string(temp->Int())).c_str());
                      }
                  }
              }

              fprintf(live_file, "} | ");

              // node->NodeInfo()->Print(live_file, reg_manager->temp_map_);

              fprintf(live_file, "{");

              if (out_) {
                  for (const auto &temp : out_->GetList()) {
                      auto name = reg_manager->temp_map_->Look(temp);

                      if (name) {
                          fprintf(live_file, "%s ", name->c_str());
                      } else {
                          fprintf(live_file, "%s ", ("t" + std::to_string(temp->Int())).c_str());
                      }
                  }
              }

              fprintf(live_file, "}\n");
          }
          /* print liveness */
      }
  }

  InterfGraph();
}

} // namespace live