#ifndef TIGER_LIVENESS_LIVENESS_H_
#define TIGER_LIVENESS_LIVENESS_H_

#include "tiger/codegen/assem.h"
#include "tiger/frame/temp.h"
#include "tiger/frame/x64frame.h"
#include "tiger/liveness/flowgraph.h"
#include "tiger/util/graph.h"
#include <unordered_map>
namespace live {

using INode = graph::Node<temp::Temp>;
using INodePtr = graph::Node<temp::Temp> *;
using INodeList = graph::NodeList<temp::Temp>;
using INodeListPtr = graph::NodeList<temp::Temp> *;
using IGraph = graph::Graph<temp::Temp>;
using IGraphPtr = graph::Graph<temp::Temp> *;

class MoveList {
public:
  MoveList() = default;

  [[nodiscard]] const std::list<std::pair<INodePtr, INodePtr>> &
  GetList() const {
    return move_list_;
  }
  
  void Append(INodePtr src, INodePtr dst) { move_list_.emplace_back(src, dst); }
  bool Contain(INodePtr src, INodePtr dst);
  void Delete(INodePtr src, INodePtr dst);
  void Prepend(INodePtr src, INodePtr dst) {
    move_list_.emplace_front(src, dst);
  }
  MoveList *Union(MoveList *list);
  MoveList *Intersect(MoveList *list);

private:
  std::list<std::pair<INodePtr, INodePtr>> move_list_;
};

struct LiveGraph {
  IGraphPtr interf_graph;
  MoveList *moves;

  LiveGraph(IGraphPtr interf_graph, MoveList *moves)
      : interf_graph(interf_graph), moves(moves) {}
};

class LiveGraphFactory {
public:
  explicit LiveGraphFactory(fg::FGraphPtr flowgraph)
      : flowgraph_(flowgraph), live_graph_(new IGraph(), new MoveList()),
        in_(std::make_unique<graph::Table<assem::Instr, temp::TempList>>()),
        out_(std::make_unique<graph::Table<assem::Instr, temp::TempList>>()),
        temp_node_map_(new tab::Table<temp::Temp, INode>()) {}
  void Liveness();
  LiveGraph GetLiveGraph() { return live_graph_; }
  tab::Table<temp::Temp, INode> *GetTempNodeMap() { return temp_node_map_; }
 
  ~LiveGraphFactory(){
      delete this->live_graph_.interf_graph;
      delete this->live_graph_.moves;
  }

private:
  /* 控制流图 */
  fg::FGraphPtr flowgraph_;

  /* 活性图 */
  LiveGraph live_graph_;
  
  /* 映射 instruction -> live_in_regs */
  std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> in_;

  /* 映射 instruction -> live_out_regs */
  std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> out_;
  tab::Table<temp::Temp, INode> *temp_node_map_;

  /* 进行寄存器活性分析 */
  void LiveMap();

  /* 根据活性分析结果进行相干图构建 */
  void InterfGraph();

  void PrintLiveOutSet(frame::RegManager *reg_manager){
      {
          /* print liveness */
          FILE *live_file = fopen("/home/stu/tiger-compiler/live_output.txt", "w");

          for (const auto &node : this->flowgraph_->Nodes()->GetList()) {
              auto in_ = this->in_->Look(node);
              auto out_ = this->out_->Look(node);

              auto instr = node->NodeInfo();

              if (typeid(*instr) == typeid(assem::OperInstr)) {
                  fprintf(live_file, "%s", static_cast<assem::OperInstr *>(instr)->assem_.c_str());
              }
              if (typeid(*instr) == typeid(assem::MoveInstr)) {
                  fprintf(live_file, "%s", static_cast<assem::MoveInstr *>(instr)->assem_.c_str());
              }
              if (typeid(*instr) == typeid(assem::LabelInstr)) {
                  fprintf(live_file, "%s", static_cast<assem::LabelInstr *>(instr)->assem_.c_str());
              }

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

          fflush(live_file);

          fclose(live_file);
      }
  }
};

} // namespace live

#endif