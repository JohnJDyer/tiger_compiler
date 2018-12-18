#pragma once
#include <map>
#include <utility>

#include "IrCodeGenerator.hpp"
#include "SemanticAnalyzer/SymbolTable.hpp"
#include "CFGGenerator.hpp"

struct Allocation {
  enum class Type {
    UNALLOCATED,
    MEMORY,          // No registered allocated for these 3 enums, use memory
    LOCAL_VARIABLE,  // fp[-]
    ARGUMENT,        // fp[+]
    REGISTER         // Register is allocated
  };

  Type type;
  union {
    int register_number;
    int slot_number;
    int argument_number;
  };
};

enum class AccessType { READ, WRITE };

struct VariableUse {
  SymbolInfoPtr symbol;
  int line;
  AccessType access;
  bool operator<(const VariableUse& other) const {
    if (line < other.line) {
      return true;
    }
    if (line == other.line) {
      if (symbol->unique_name < other.symbol->unique_name) {
        return true;
      }
      if (symbol->unique_name == other.symbol->unique_name) {
        return access < other.access;
      }
    }
    return false;
  }
};

/**
 * Actions that should happen before or after an instruction.
 * For example, before the basic block starts, there should be some fills.
 * Right after the basic block ends, there should be some spills.
 */
struct ExtraAction {
  enum class Action { SPILL, FILL } type;
  SymbolInfoPtr symbol;
  int           register_number;
};

using ExtraActionList = std::vector<ExtraAction>;

class AllocationMap {
 public:
  using Iterator = std::map<VariableUse, Allocation>::const_iterator;
  virtual ~AllocationMap() = default;
  virtual void set(SymbolInfoPtr symbol, int line, AccessType access, Allocation alloc);
  virtual void add_preaction (int line, ExtraAction::Action action, SymbolInfoPtr symbol, int register_number);
  virtual void add_postaction(int line, ExtraAction::Action action, SymbolInfoPtr symbol, int register_number);
  // Actions that should happen before the instruction executes.
  virtual std::shared_ptr<ExtraActionList> get_preactions(int line) const;
  // Actions that should happen after the instruction executes.
  virtual std::shared_ptr<ExtraActionList> get_postactions(int line) const;
  virtual Allocation get(SymbolInfoPtr symbol, int line,  AccessType access) const;

 private:
  std::map<VariableUse, Allocation> map_;
  std::map<int, std::shared_ptr<ExtraActionList>> pre_actions_;
  std::map<int, std::shared_ptr<ExtraActionList>> post_actions_;
};


class IntraBlockAllocationMap : public AllocationMap {
 public:
  IntraBlockAllocationMap(CFG cfg, InstructionMap inst_map){

    for(auto bb_graph: cfg.block_graphs){
      //cout << "=================\n";
      for(auto range_register: bb_graph.second -> allocation2){
        if(range_register.second != 0){ //is allocated


          int start_line = inst_map[range_register.first -> accesses.front()];
          int end_line   = inst_map[range_register.first -> accesses.back()];

          if(range_register.first -> write == true){
            //IF THERE IS A WRITE THEN STORE IT
            add_postaction(
              end_line,
              ExtraAction::Action::SPILL,
              range_register.first -> symbol,
              range_register.second);

          }else {
            //IF NO WRITE THEN LOAD IT
            add_preaction(
              start_line,
              ExtraAction::Action::FILL,
              range_register.first -> symbol,
              range_register.second);
          }

          for(auto i = start_line; i <= end_line; i++){

            Allocation alloc;
            alloc.type            = Allocation::Type::REGISTER;
            alloc.register_number = range_register.second;
            line_symbol_allocations[i][range_register.first -> symbol] = alloc;
          }
        }
      }
    }

    /*
      add_preaction (16, ExtraAction::Action::FILL,  cfg.test,1);
      add_postaction(17, ExtraAction::Action::SPILL, cfg.test,1);
    */
  }

  map<int,map<SymbolInfoPtr,Allocation>> line_symbol_allocations;

  virtual Allocation get(
    SymbolInfoPtr symbol,
    int           line,
    AccessType    access
  ) const override {

    Allocation ret_alloc;

    if(line_symbol_allocations.find(line) != line_symbol_allocations.end()){
      auto symbol_alloc = line_symbol_allocations.at(line);
      if(symbol_alloc.find(symbol) != symbol_alloc.end()){
        return symbol_alloc.at(symbol);
      }
    }

    switch (symbol->category) {
      case SymbolCategory::VARIABLE:
      case SymbolCategory::TEMPORARY:
        // No local variable
        ret_alloc.type            = Allocation::Type::MEMORY;
        break;
      case SymbolCategory::ARGUMENT:
        ret_alloc.type            = Allocation::Type::ARGUMENT;
        ret_alloc.argument_number = symbol -> argument_position;
        break;
      default:
        ret_alloc.type            = Allocation::Type::UNALLOCATED;
    }
    return ret_alloc;
  }
};


class TestAllocationMap : public AllocationMap {
 public:
  TestAllocationMap(CFG &cfg){

    SymbolInfoPtr test;

    for(auto asdf: cfg.intra_block_ranges_by_blocks_2){
      for(auto sym_whatever: asdf.second)
        if(sym_whatever.first->local_name == "sum"){
          test = sym_whatever.first;
        }
    }

    add_preaction (16, ExtraAction::Action::FILL,  test,1);
    add_preaction (16, ExtraAction::Action::FILL,  test,1);
    add_preaction (16, ExtraAction::Action::FILL,  test,1);
    add_preaction (16, ExtraAction::Action::FILL,  test,1);
    add_preaction (16, ExtraAction::Action::FILL,  test,1);

    add_postaction(16, ExtraAction::Action::SPILL, test,1);
    add_postaction(16, ExtraAction::Action::SPILL, test,1);
    add_postaction(16, ExtraAction::Action::SPILL, test,1);
    add_postaction(16, ExtraAction::Action::SPILL, test,1);
    add_postaction(16, ExtraAction::Action::SPILL, test,1);

  }


  virtual Allocation get(
    SymbolInfoPtr symbol,
    int           line,
    AccessType    access
  ) const override {

    Allocation ret_alloc;


    switch (symbol->category) {
      case SymbolCategory::VARIABLE:
      case SymbolCategory::TEMPORARY:
        // No local variable
        ret_alloc.type            = Allocation::Type::MEMORY;
        break;
      case SymbolCategory::ARGUMENT:
        ret_alloc.type            = Allocation::Type::ARGUMENT;
        ret_alloc.argument_number = symbol->argument_position;
        break;
      default:
        ret_alloc.type            = Allocation::Type::UNALLOCATED;
    }
    return ret_alloc;
  }
};



class NaiveAllocationMap : public AllocationMap {
 public:
  virtual Allocation get(
    SymbolInfoPtr symbol,
    int           line,
    AccessType    access
  ) const override {

    Allocation ret_alloc;

    switch (symbol->category) {
      case SymbolCategory::VARIABLE:
      case SymbolCategory::TEMPORARY:
        // No local variable
        ret_alloc.type            = Allocation::Type::MEMORY;
        break;
      case SymbolCategory::ARGUMENT:
        ret_alloc.type            = Allocation::Type::ARGUMENT;
        ret_alloc.argument_number = symbol->argument_position;
        break;
      default:
        ret_alloc.type            = Allocation::Type::UNALLOCATED;
    }
    return ret_alloc;
  }
};

class RegisterAllocator {
 public:
  virtual ~RegisterAllocator() = default;
  virtual void allocate() = 0;
  virtual const AllocationMap& get_alloc_map() const = 0;
};

class NaiveRegisterAllocator : public RegisterAllocator {
 public:
  NaiveRegisterAllocator() {}
  virtual void allocate() override {}
  virtual const AllocationMap& get_alloc_map() const override {
    return alloc_map;
  }

 private:
  NaiveAllocationMap alloc_map;
};
