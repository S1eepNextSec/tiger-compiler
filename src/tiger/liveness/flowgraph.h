#ifndef TIGER_LIVENESS_FLOWGRAPH_H_
#define TIGER_LIVENESS_FLOWGRAPH_H_

#include "tiger/codegen/assem.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/util/graph.h"

#include <map>
namespace fg {

using FNode = graph::Node<assem::Instr>;
using FNodePtr = graph::Node<assem::Instr> *;
using FNodeListPtr = graph::NodeList<assem::Instr> *;
using FGraph = graph::Graph<assem::Instr>;
using FGraphPtr = graph::Graph<assem::Instr> *;

/* 构造控制流图的工厂类 */
class FlowGraphFactory {
public:
  explicit FlowGraphFactory(assem::InstrList *instr_list)
      : instr_list_(instr_list), flowgraph_(new FGraph()),
        label_map_(std::make_unique<std::map<std::string, FNode *>>()) {}
  
  /* 构造控制流图 */
  void AssemFlowGraph();
  
  FGraphPtr GetFlowGraph() { return flowgraph_; }

private:
  /* 指令列表 */
  assem::InstrList *instr_list_;
  
  /* 控制流图 */
  FGraphPtr flowgraph_;
  
  /* 映射 label -> node , 辅助完成控制流构建 */
  std::unique_ptr<std::map<std::string, FNode *>> label_map_;
};

} // namespace fg

#endif