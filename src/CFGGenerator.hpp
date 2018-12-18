#pragma once
#include <iostream>
#include <map>
#include <utility>
#include <algorithm>
#include <limits>
#include <set>
#include <numeric>

using namespace std;

class BasicBlock {
 public:
  BasicBlock(string label): label(label){}

  void append_instruction(shared_ptr<Instruction> inst){
    instructions.push_back(inst);
  }

  string          label;
  InstructionList instructions;
};

struct IntraBlockRange2{

  IntraBlockRange2(){}
  SymbolInfoPtr   symbol    = nullptr;
  bool            write     = false;
  InstructionList accesses;

  void append_read(SymbolInfoPtr sym, shared_ptr<Instruction> inst_ptr){
    assert(symbol == nullptr or sym==symbol);
    accesses.push_back(inst_ptr);
    symbol = sym;
  }

  void append_write(SymbolInfoPtr sym, shared_ptr<Instruction> inst_ptr){
    assert(accesses.size() == 0);
    assert(symbol == nullptr or sym==symbol);
    write = true;
    accesses.push_back(inst_ptr);
    symbol = sym;
  }

  bool overlap(IntraBlockRange2 other, InstructionMap & inst_map){
    if(accesses.size() >= 1){
      return !(inst_map[other.accesses.back()] <= inst_map[      accesses.front()] or
               inst_map[      accesses.back()] <= inst_map[other.accesses.front()]);
    }
  }

  int const spill_cost (){
    return accesses.size() - 1;
  }

};

struct LiveRangeAtom{
  LiveRangeAtom(){}
  LiveRangeAtom(bool top_open, bool bottom_open, int ray_top, int ray_bottom):
  top_open(top_open), bottom_open(bottom_open),ray_top(ray_top),ray_bottom(ray_bottom) {}

  bool top_open, bottom_open;
  int  ray_top, ray_bottom;

  const bool overlap(const LiveRangeAtom other){
    return true;
    //(top_open     and other.top_open   ) or
    //(bottom_open  and other.bottom_open) or
    //(!bottom_open and !top_open        )
  }
};

using BasicBlockPtr       = shared_ptr<BasicBlock>;
using IntraBlockRange2Ptr = shared_ptr<IntraBlockRange2>;
using BasicBlockList      = vector    <BasicBlockPtr>;

class LiveRange {
 public:
  LiveRange(SymbolInfoPtr symbol, BasicBlockPtr basic_block, int ray_top, int ray_bottom):
    symbol(symbol){
    basic_blocks.insert(basic_block);
    partial_blocks[basic_block] = LiveRangeAtom(false, false, ray_top, ray_bottom);
  }

  LiveRange(SymbolInfoPtr symbol):
    symbol(symbol){
  }

  SymbolInfoPtr                     symbol;
  set<BasicBlockPtr>                basic_blocks;
  map<BasicBlockPtr, LiveRangeAtom> partial_blocks;

  bool overlap(LiveRange &other){
    vector<BasicBlockPtr> bb_intersection;

    set_intersection(
      basic_blocks.begin(),
      basic_blocks.end(),
      other.basic_blocks.begin(),
      other.basic_blocks.end(),
      bb_intersection.begin());

    //could check once
    for(auto bb: bb_intersection){
      if(        partial_blocks.find(bb) ==       partial_blocks.end()
                 or other.partial_blocks.find(bb) == other.partial_blocks.end())  {
        return true;
      }
    }

    for(auto bb: bb_intersection){
      if(partial_blocks[bb].overlap(other.partial_blocks[bb])){
        return true;
      }
    }
    return false;
  }

};

using LiveRangePtr       = shared_ptr<LiveRange>;
using LiveRangeList      = vector    <LiveRangePtr>;
using LiveRangeAtomPtr   = shared_ptr<LiveRangeAtom>;
using LiveRangeAtomList  = vector    <LiveRangeAtomPtr>;

//template or something
class InterferenceGraph {
 public:
  InterferenceGraph(
    map<SymbolInfoPtr,
    vector<IntraBlockRange2Ptr>> symbols_to_ranges,
    InstructionMap & inst_map,
    int start_register, int end_register){

    /* O(n^2) ranges; A more efficient approach is possible with online graph generation
     * during range generation. The alternative approach exploits the order on range generation;
     * However, it is less encapsulated -> more prone to error. Will implement iff
     * time permits.
     */
    for(auto sym_ranges: symbols_to_ranges){
      for(auto range: sym_ranges.second){
        ranges_to_symbols2[range] = sym_ranges.first;
        ranges2.push_back(range);
        for(auto other_sym_ranges: symbols_to_ranges){
          for(auto other_range: other_sym_ranges.second){
            if(range -> overlap(*other_range, inst_map)){
              interferences2[range].insert(other_range);
            }
          }
        }
      }
    }
    begin_safe_ranges2 = ranges2.end();

    //partition_safe_ranges(end_register - start_register);
    prioritize_ranges();
    //simplify_dependencies();
    naive_allocate(start_register,end_register);
  }

  void partition_safe_ranges(int register_count){
    begin_safe_ranges2 = partition(
      ranges2.begin(),
      ranges2.end(),
      [this, register_count] (IntraBlockRange2Ptr range) -> bool {
        this->interferences2[range].size() >= register_count; }
    );
  }

  void prioritize_ranges(){

    sort(
      ranges2.begin(),
      begin_safe_ranges2,
      [] (IntraBlockRange2Ptr lhs, IntraBlockRange2Ptr rhs) -> bool {
        lhs->spill_cost() >
        rhs->spill_cost();});
  }

  void simplify_dependencies(){
    set<IntraBlockRange2Ptr>range_set(ranges2.begin(),ranges2.end());
    for(auto range:ranges2){
      range_set.erase(range);
      interferences2[range].erase(
        range_set.begin(),
        range_set.end());
    }
  }

  void naive_allocate(int start_register, int end_register){
    for(auto range:ranges2){
      allocation2[range] = start_register * static_cast<int>( start_register<=end_register);
      start_register ++;
      if(start_register >= end_register){
        return;
      }
    }
  }

  map<IntraBlockRange2Ptr,int> allocation2; //zero is no allocation

 private:
  map<IntraBlockRange2Ptr, set<IntraBlockRange2Ptr>> interferences2;
  map<IntraBlockRange2Ptr, SymbolInfoPtr>            ranges_to_symbols2;
  vector<IntraBlockRange2Ptr>                        ranges2;
  vector<IntraBlockRange2Ptr>::iterator              begin_safe_ranges2;
};

class CFG {
 public:

  CFG(){}
  CFG(const InstructionList& insns, InstructionMap& inst_map, int start_register = 4, int end_register = 23) {

    root = next_block("__header");

    for (auto i: insns) {
      if (i->topology() < 0) {
        append_symbol_accesses(i);
      } else if (i->topology() == 0){ //new label
        create_cfg_child_immediate_block(i->get_label());
        flush_symbol_accesses();
        next_block(i->get_label());
      } else if (i->topology() == 1){ //unconditional jump
        flush_symbol_accesses();
        new_block_anon();
      } else if (i->topology() == 2) { //branch
        append_symbol_accesses(i);
        flush_symbol_accesses();
        create_cfg_child_immediate_block(i->get_label());
        create_cfg_child_immediate_block(new_block_anon());
      }
    }

    flush_symbol_accesses();
    construct_interference_graphs(inst_map);

  }

  map<BasicBlockPtr,shared_ptr<InterferenceGraph>>  block_graphs;
  int start_register, end_register;

  void construct_interference_graphs(InstructionMap& inst_map){
    for(auto bb_symbol_ranges: intra_block_ranges_by_blocks_2){
      block_graphs[bb_symbol_ranges.first]
        = make_shared<InterferenceGraph>(bb_symbol_ranges.second, inst_map, start_register, end_register);
    }
  }

  void flush_symbol_accesses(){
    for(auto i : l_symbol_access)
      generate_intra_block_range(i.first);
    l_symbol_access.clear();
  }

  void generate_intra_block_range(SymbolInfoPtr sym){
    if(l_symbol_access.find(sym) != l_symbol_access.end()){
      intra_block_ranges_by_blocks_2[current_block()][sym].push_back(
        make_shared<IntraBlockRange2>(l_symbol_access[sym]));
      l_symbol_access.erase(sym);
    }
  }

  void create_cfg_child_immediate_block(string child_label){
    cfg_parent_to_children[current_block()].push_back(basic_blocks[child_label]);
  }

  void create_cfg_child_immediate_block(BasicBlockPtr child_bb){
    cfg_parent_to_children[current_block()].push_back(child_bb);
  }

  //if a=a+a then range is split and read duplicates
  void append_symbol_accesses(shared_ptr<Instruction> inst){
    current_block() -> append_instruction(inst);

    for(auto sym: inst->get_reads()) {
      l_symbol_access[sym].append_read(sym,inst);
    }

    if(inst->can_write()){
      auto write_sym = inst->get_write();
      generate_intra_block_range(write_sym);
      l_symbol_access[write_sym].append_write(write_sym,inst);
    }
  }

  void bb_traversal(){}

  LiveRangeList generate_live_ranges(){
    LiveRangeList     live_ranges;
    LiveRangeAtomList atoms; //TODO: Change to bb_symbol map or something

    for(auto bb_sym_range: intra_block_ranges_by_blocks_2){
      for(auto sym_ranges: bb_sym_range.second){

        //remove the middles;
        if(sym_ranges.second.size() > 2){

          auto start = sym_ranges.second.begin() + 1;
          auto end   = sym_ranges.second.end()   - 1;

          for(auto it = start; it != end; it++){
            //TODO: infer locations;
            live_ranges.push_back(
              make_shared<LiveRange>( sym_ranges.first, bb_sym_range.first, -314, -314)
            );
          }
          sym_ranges.second.erase(start,end);
        }

        if(sym_ranges.second.size() == 2){
          auto first_atom = sym_ranges.second[0];
          if(first_atom ->write){
            live_ranges.push_back(
              make_shared<LiveRange>(first_atom->symbol, bb_sym_range.first, -314, -314)
            );
          } else {
            LiveRangeAtom(true, false, 0, -314);    //TODO: track this guy
          }
        }

        if(sym_ranges.second.size() == 1){
          auto first_atom = sym_ranges.second[0];
          if(first_atom ->write){
            LiveRangeAtom(false, true, -314, -314); //TODO: track this guy
          } else {
            LiveRangeAtom(true, true, 0, -314);     //TODO: track this guy
          }
        }
      }
    }
    return live_ranges;
  }

  map<BasicBlockPtr,map<SymbolInfoPtr,vector<IntraBlockRange2Ptr>>> intra_block_ranges_by_blocks_2;

 private:
  BasicBlockPtr                         root;
  map<SymbolInfoPtr,IntraBlockRange2>   l_symbol_access;
  vector<string>                        labels;
  map<string,BasicBlockPtr>             basic_blocks;

  //map<BasicBlockPtr,vector<BasicBlockPtr>>    cfg_child_to_parents;   // might need both, might drop one later
  map<BasicBlockPtr,vector<BasicBlockPtr>>      cfg_parent_to_children; // Drop _goto_orphan, header has main child

  int anon_block_count    = 0;

  BasicBlockPtr current_block(){
    return basic_blocks[labels.back()];
  }

  BasicBlockPtr new_block_anon(){
    return next_block("anon_" + to_string(anon_block_count++));
  }

  BasicBlockPtr next_block(string label){
    labels.push_back(label);
    basic_blocks[label] = make_shared<BasicBlock>(label);
    return basic_blocks[label];
  }

};

