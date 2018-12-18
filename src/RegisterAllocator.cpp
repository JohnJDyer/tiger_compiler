#include "RegisterAllocator.hpp"

#include <memory>
#include <vector>

#include "SemanticAnalyzer/SymbolTable.hpp"

using namespace std;

namespace {

void add_action_to_map(
    map<int, shared_ptr<ExtraActionList>>& actions,
    int                 line,
    ExtraAction::Action type,
    SymbolInfoPtr       symbol,
    int                 register_number) {
  ExtraAction the_action;
  the_action.type             = type;
  the_action.symbol           = symbol;
  the_action.register_number  = register_number;
  if (!actions[line]) {
    actions[line] = make_shared<ExtraActionList>();
  }
  actions[line]->push_back(the_action);
}

shared_ptr<ExtraActionList> get_actions_from_map(
    const map<int, shared_ptr<ExtraActionList>>& actions, int line) {
  map<int, shared_ptr<ExtraActionList>>::const_iterator found =
      actions.find(line);
  if (found == actions.end()) {
    return make_shared<ExtraActionList>();
  }
  return found->second;
}

}  // namespace

void AllocationMap::set(SymbolInfoPtr symbol, int line, AccessType access,
                        Allocation alloc) {
  VariableUse var_use;
  var_use.symbol  = symbol;
  var_use.line    = line;
  var_use.access  = access;
  map_[var_use]   = alloc;
}

Allocation AllocationMap::get(SymbolInfoPtr symbol, int line,
                              AccessType access) const {

  //std::cout << "Touched line: " << line << " " << "\n";

  VariableUse var_use;
  var_use.symbol  = symbol;
  var_use.line    = line;
  var_use.access  = access;
  map<VariableUse, Allocation>::const_iterator found;
  found = map_.find(var_use);
  if (found == map_.end()) {
    Allocation ret;
    ret.type = Allocation::Type::UNALLOCATED;
    return ret;
  }
  return found->second;
}

void AllocationMap::add_preaction(int line, ExtraAction::Action action,
                                  SymbolInfoPtr symbol, int register_number) {
  add_action_to_map(pre_actions_, line, action, symbol, register_number);
}

void AllocationMap::add_postaction(int line, ExtraAction::Action action,
                                   SymbolInfoPtr symbol, int register_number) {
  add_action_to_map(post_actions_, line, action, symbol, register_number);
}

shared_ptr<ExtraActionList> AllocationMap::get_preactions(int line) const {
  return get_actions_from_map(pre_actions_, line);
}

shared_ptr<ExtraActionList> AllocationMap::get_postactions(int line) const {
  return get_actions_from_map(post_actions_, line);
}
