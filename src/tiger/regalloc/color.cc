#include "tiger/regalloc/color.h"

extern frame::RegManager *reg_manager;
extern std::map<std::string, std::pair<int, int>> frame_info_map;
namespace col {
/* TODO: Put your lab6 code here */
    void Color::PreColor(){
        /* 获取可使用的寄存器集合 */
        auto machine_regs = reg_manager->Registers();
        auto rsp_temp = reg_manager->GetRegister(frame::X64RegManager::Reg::RSP);
        machine_regs->Append(rsp_temp);

        /* 设定可分配寄存器数量 K = 16 */
        this->K = machine_regs->GetList().size();

        int pre_color = 1;

        /* 找出已经构建Node的Machine Regs */
        for (const auto & inode : this->graph->Nodes()->GetList()){
            auto temp = inode->NodeInfo();

            if (machine_regs->Contain(temp)){
                this->machine_regs.insert(inode);

                /* 进行预染色 */
                this->reg_color_map.insert({inode, pre_color++});
            }
        }
    }

    std::vector<live::INodePtr> Color::doColor(){
        this->InitNodeAdjsMapping();
        this->PreColor();
        this->InitWorkSet();

        while ( this->low_degree_simplify_set.size() != 0 ||
                this->potential_moves.size() != 0 ||
                this->low_degree_move_related_set.size() !=0 ||
                this->high_degree_set.size() != 0 
                ){
            
            if (this->low_degree_simplify_set.size()){
                this->Simplify();
            } else if (this->potential_moves.size()){
                this->Coalesce();
            } else if (this->low_degree_move_related_set.size()){
                this->Freeze();
            } else if (this->high_degree_set.size()){
                this->Spill();
            }
        }

        this->AssignColor();

        std::vector<live::INodePtr> result;

        for (const auto & spill_node : this->high_degree_set){
            result.push_back(spill_node);
        }

        return result;
    }

    void Color::InitNodeAdjsMapping(){
        /* 遍历每个 Node */
        for (const auto &inode : this->graph->Nodes()->GetList()) {
            if (isMachineReg(inode)) {
                continue;
            }

            auto preds = inode->Pred()->GetList();
            auto succs = inode->Succ()->GetList();

            /* 构建 node -> intefere edges 映射*/
            if (this->node_interfere_edge_map.find(inode) == this->node_interfere_edge_map.end()){
                std::unordered_set<live::INodePtr> adj_set;
                this->node_interfere_edge_map.insert({inode, std::move(adj_set)});
            }

            for (const auto &adj : preds) {
                this->node_interfere_edge_map[inode].insert(adj);
            }

            for (const auto &adj : succs) {
                this->node_interfere_edge_map[inode].insert(adj);
            }
        }
        /* 所有非机器寄存器节点都存储了 节点 -> 邻居(包含机器寄存器节点) 的映射*/

        /* 根据每个节点邻居数 计算初始度 */
        for (const auto & inode : this->graph->Nodes()->GetList()){
            if (isMachineReg(inode)){
                this->node_current_degree[inode] = this->K;
            } else {
                this->node_current_degree[inode] = this->node_interfere_edge_map.at(inode).size();
            }
        }

    }

    void Color::InitWorkSet(){
        /* FIXME: 保证 move_list 里面没有重复的对 */
        for (auto move_pair : this->move_list){
            if (this->node_move_instr_map.find(move_pair->first) == this->node_move_instr_map.end()){
                this->node_move_instr_map.insert({move_pair->first, std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *>()});
            }
            if (this->node_move_instr_map.find(move_pair->second) == this->node_move_instr_map.end()) {
                this->node_move_instr_map.insert({move_pair->second, std::unordered_set<std::pair<live::INodePtr, live::INodePtr> *>()});
            }

            this->node_move_instr_map[move_pair->first].insert(move_pair);
            this->node_move_instr_map[move_pair->second].insert(move_pair);

            this->unprepared_moves.insert(move_pair);
        }

        for (const auto &inode : this->graph->Nodes()->GetList()) {
            if (isMachineReg(inode)) {
                continue;
            }

            /* 根据初始的度构建工作集 */
            if (isLowDegree(inode)) {
                if (isMoveRelated(inode)) {
                    this->low_degree_move_related_set.insert(inode);
                } else {
                    this->low_degree_simplify_set.insert(inode);
                }
            } else {
                this->high_degree_set.insert(inode);
            }
        }
    }

    void Color::Simplify(){
        /* 迭代过程中 simpilify集合 可能加入新的元素 */
        while (this->low_degree_simplify_set.size()){
            auto node_iter = this->low_degree_simplify_set.begin();
            
            this->select_stack.push_back(*node_iter);

            auto adj_vec = this->GetCurrentActiveAdjacent(*node_iter);

            for (const auto & adj : adj_vec){
                this->DecrementDegree(adj);
            }

            this->low_degree_simplify_set.erase(node_iter);
        }
    }

    void Color::DecrementDegree(live::INodePtr inode){
        if (isMachineReg(inode)){
            return;
        }

        this->node_current_degree[inode]--;

        /* 从 high degree -> low degree */
        if (this->node_current_degree[inode] == this->K - 1){
            this->AddPotentialMoves(inode);

            auto adj_list = this->GetCurrentActiveAdjacent(inode);

            for (auto adj : adj_list){
                this->AddPotentialMoves(adj);
            }

            this->high_degree_set.erase(inode);

            if (isMoveRelated(inode)){
                this->low_degree_move_related_set.insert(inode);
            } else {
                this->low_degree_simplify_set.insert(inode);
            }
        }
    }

    void Color::AddPotentialMoves(live::INodePtr p){
        auto moves = this->GetRelatedMoves(p);

        for (auto move : moves){
            if (this->unprepared_moves.find(move) != this->unprepared_moves.end()){
                this->unprepared_moves.erase(move);
                this->potential_moves.insert(move);
            }
        }
    }

    void Color::Coalesce(){
        auto add_work_list = [this](live::INodePtr p) {
            if (!this->isPreColored(p) && 
                !this->isMoveRelated(p) && 
                this->isLowDegree(p)){
                    this->low_degree_move_related_set.erase(p);
                    this->low_degree_simplify_set.insert(p);
            }
        };

        while (this->potential_moves.size()) {
            auto potential_move = * (this->potential_moves.begin());
            this->potential_moves.erase(potential_move);

            auto move_src = potential_move->first;
            auto move_dst = potential_move->second;

            move_src = this->GetAlias(move_src);
            move_dst = this->GetAlias(move_dst);

            live::INodePtr combine_source = move_src;
            live::INodePtr combine_target = move_dst;

            if (this->isPreColored(move_src)){
                combine_target = move_src;
                combine_source = move_dst;
            }

            if (combine_target == combine_source){
                coalesced_moves.insert(potential_move);
                add_work_list(combine_target);
            } else if ((this->isPreColored(combine_source) && this->isPreColored(combine_target)) ||
                        this->hasInterfereEdge(combine_source,combine_target)) {    
                this->constrained_moves.insert(potential_move);
                add_work_list(combine_target);
                add_work_list(combine_source);
            } else if ((this->isPreColored(combine_target) && GeorgeRuleCheck(combine_source, combine_target)) ||
                       (!this->isPreColored(combine_target) && BriggsRuleCheck(combine_source, combine_target))) {
                this->coalesced_moves.insert(potential_move);
                // COMBINE
                this->Combine(combine_target, combine_source);

                add_work_list(combine_target);
            } else {
                this->unprepared_moves.insert(potential_move);
            }
        }
    }

    void Color::Combine(live::INodePtr target, live::INodePtr source){
        if (this->low_degree_move_related_set.find(source) != this->low_degree_move_related_set.end()){
            this->low_degree_move_related_set.erase(source);
        } else {
            this->high_degree_set.erase(source);
        }

        this->coalesced_nodes.insert(source);

        this->alias_map.insert({source, target});

        const auto &source_move_instrs = this->node_move_instr_map[source];

        for (const auto &move_instr : source_move_instrs) {
            this->node_move_instr_map[target].insert(move_instr);
        }

        this->AddPotentialMoves(target);

        auto source_adjs = this->GetCurrentActiveAdjacent(source);

        for (const auto & adj: source_adjs){
            this->AddEdge(target, adj);
            this->DecrementDegree(adj);
        }

        if (this->isHighDegree(target) && 
            this->low_degree_move_related_set.find(target) != this->low_degree_move_related_set.end()){
            this->low_degree_move_related_set.erase(target);
            this->high_degree_set.insert(target);
        }
    }

    void Color::AddEdge(live::INodePtr a, live::INodePtr b){
        if (!this->isMachineReg(a)){
            this->node_interfere_edge_map[a].insert(b);
            this->node_current_degree[a]++;
        }

        if (!this->isMachineReg(b)){
            this->node_interfere_edge_map[b].insert(a);
            this->node_current_degree[b]++;
        }
    }

    bool Color::BriggsRuleCheck(live::INodePtr a, live::INodePtr b){
        auto a_adjs = this->GetCurrentActiveAdjacent(a);
        auto b_adjs = this->GetCurrentActiveAdjacent(b);

        std::unordered_set<live::INodePtr> adjs;

        for (const auto & node : a_adjs){
            adjs.insert(node);
        }

        for (const auto & node : b_adjs){
            adjs.insert(node);
        }

        int high_degree_count = 0;

        for (const auto & node : adjs){
            if (this->isHighDegree(node)){
                high_degree_count++;
            }
        }

        return high_degree_count < this->K;
    }
    bool Color::GeorgeRuleCheck(live::INodePtr source, live::INodePtr target){
        // return true;
        auto adjs = this->GetCurrentActiveAdjacent(source);

        for (const auto & node : adjs){
            if (this->isLowDegree(node) || 
                this->hasInterfereEdge(node,target)){
                continue;
            }

            return false;
        }

        return true;
    }

    void Color::Freeze(){
        auto inode = * this->low_degree_move_related_set.begin();

        this->low_degree_move_related_set.erase(inode);

        this->low_degree_simplify_set.insert(inode);

        this->FreezeRelatedMoves(inode);
    }

    void Color::FreezeRelatedMoves(live::INodePtr p){
        const auto &related_moves = this->GetRelatedMoves(p);

        for (const auto &move : related_moves) {
            live::INodePtr another_node = nullptr;

            if (this->GetAlias(move->first) == this->GetAlias(p)) {
                another_node = this->GetAlias(move->second);
            } else {
                another_node = this->GetAlias(move->first);
            }

            this->unprepared_moves.erase(move);
            this->frozen_moves.insert(move);

            if (!this->isMoveRelated(another_node) && this->isLowDegree(another_node)) {
                this->low_degree_move_related_set.erase(another_node);
                this->low_degree_simplify_set.insert(another_node);
            }
        }
    }
    
    void Color::Spill(){
        auto spill_node = this->SelectNodeToSpill();

        this->high_degree_set.erase(spill_node);

        this->low_degree_simplify_set.insert(spill_node);

        this->FreezeRelatedMoves(spill_node);
    }

    live::INodePtr Color::SelectNodeToSpill() {
        int max = 0;
        live::INodePtr node_to_spill = nullptr;
        for (const auto &inode : this->high_degree_set) {
            if (this->node_current_degree[inode] > max){
                node_to_spill = inode;
                max = this->node_current_degree[inode];
            }
        }

        return node_to_spill;
    }

    void Color::AssignColor(){
        while (this->select_stack.size()){
            auto node_to_color = this->select_stack.back();

            this->select_stack.pop_back();

            std::set<int> valid_colors;

            for (int i = 1; i <= this->K;i++){
                valid_colors.insert(i);
            }

            // FIXME: 用 CurrentActiveAdj 会引起问题
            // 假设这样一种情况 a & b 相干, 且有 b,c move
            // a先被simplify 先入栈 而b c之后由于可以move 出现了 b move to c
            // 因此 b 中存活邻居节点不包含a 所以 a 和 c不会有互相相干的信息
            // 完全因为 a 先被simplify 因此 必须获取所有相干关系而不是当前存活的相干关系
            // auto adjs = this->GetCurrentActiveAdjacent(node_to_color);

            const auto & adjs = this->node_interfere_edge_map.at(node_to_color);

            for (const auto & adj : adjs){
                auto actual_node = this->GetAlias(adj);

                if (this->isColored(actual_node)){
                    valid_colors.erase(this->reg_color_map[actual_node]);
                }
            }

            if (valid_colors.size() == 0){
                this->high_degree_set.insert(node_to_color);
            } else {
                auto selected_color = * valid_colors.begin();

                this->reg_color_map.insert({node_to_color, selected_color});
            }
        }
    }

    col::Result Color::DumpColorMapping(){
        std::unordered_map<int, std::string*> color_reg_map;
        auto map_result = temp::Map::Empty();

        auto unused_machine_reg_list = reg_manager->Registers();

        for (const auto & machine_reg_inode : this->machine_regs){
            auto color = this->reg_color_map[machine_reg_inode];

            auto machine_reg = machine_reg_inode->NodeInfo();

            unused_machine_reg_list->Delete(machine_reg);

            auto reg_name = reg_manager->temp_map_->Look(machine_reg);

            color_reg_map.insert({color, reg_name});
        }

        for (int i = 1; i <= this->K;i++){
            if (color_reg_map.find(i) != color_reg_map.end()){
                continue;
            }

            auto machine_reg = *unused_machine_reg_list->GetList().begin();

            auto reg_name = reg_manager->temp_map_->Look(machine_reg);

            unused_machine_reg_list->Delete(machine_reg);

            color_reg_map.insert({i, reg_name});

            map_result->Enter(machine_reg, reg_name);
        }

        for (const auto & node : this->graph->Nodes()->GetList()){
            map_result->Enter(node->NodeInfo(), color_reg_map[this->reg_color_map[this->GetAlias(node)]]);
        }

        col::Result result;

        result.coloring = map_result;
        result.spills = nullptr;

        return result;
    }
} // namespace col
/**
 * --------------------------------- Init ---------------------------------
 * · 构建控制流
 * · 根据控制流，进行活性分析，得到 Instruction -> in_set & out_set
 * · 根据 out_set 构建相干图，根据 Instruction 构建 Move Instr List
 * · 构建工作集
 *   · 将相干图中非 MoveRelated & degree < K 的节点加入 SimplifySet
 *   · 将相干图中 MoveRelated & degree < K 的节点加入到 MoveRelatedSet
 *   · 将相干图中 degree >= K 的节点加入到 PotentialSpillSet
 * · 循环
 *   · Simplify Phase
 *      · 从 SimplifySet 中删除
 *      · 对于其的每一个邻居节点
 *          · degree = degree - 1
 *          · 如果 degree < K & MoveRelated , 将对应点移动 PotentialSpillSet -> MoveRelatedSet
 *          · 如果 degree < K & !MoveRelated , 将对应点移动 PotentialSpillSet -> SimplifySet
 *   · Coalesce Phase
 *      · Coalesce Rule
 *          · Briggs: 对于两个节点 u,v , ∣{n∈( adj(u) ∪ adj(v) ) ∣ degree(n) ≥ K }∣< K 时可以进行合并
 *          · George: 若将节点 u 合并到 v , 要求 u 的每个邻接点必须要么和 v 有边，要么度小于 K
 *      · 对于普通寄存器采用 Briggs , 而普通寄存器到机器寄存器的合并采用 George,
 *        Briggs的必要条件是两个节点各自的度都小于 K , 所以当一条Move Instr中涉及的两个寄存器
 *        各自都度数小于 K , 尝试进行合并,当然可以暴力检查每一条 Move 语句
 *   · Freeze Phase
 *      · 当SimplifySet为空，而MoveRelatedSet仍然存在节点，但是已经不能继续Coalesce
 *      · 对不能Coalesce的节点从MoveRelatedSet删去，并入SimplifySet
 *   · Spill Phase
 *      · 最终剩下度数仍 >= K 的节点
 *      · 启发式选中一个节点，从PotentialSpillSet中删去，对应节点的邻节点度减少
 *      · 与该节点相关的 Move Instr 进行 Freeze
 *      · 检查邻节点是否有可以加入到 SimplifySet的节点
 *   · Simplify Phase执行知道 SimplifySet为空
 *     Coalesce Phase 尽可能执行多的Coalesce操作，一旦执行过 Coalesce 操作就回到Simplify Phase
 *     Freeze Phase 寻找一个传送指令进行冻结，回到 Simplify
 *     Spill Phase 寻找一个节点进行溢出，回到Simplify
 *   · 对于 Move Instr ,
 *     一开始所有 Move Instr 都放在 unprepared_move_instr_set 中
 *     一旦一个 Move 相关的节点度减小到 < K , 这个节点关联的所有 Move Instr 加入到 potential_coalesce_instr_set
 *     当 Simplify & Coalesce 都无法进行时, 就必须选择一条 Instr 进行 Freeze
 */