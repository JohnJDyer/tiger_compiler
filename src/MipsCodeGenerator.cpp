#include "MipsCodeGenerator.hpp"

#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <set>
#include <sstream>
#include <vector>

#include "IrCodeGenerator.hpp"
#include "RegisterAllocator.hpp"

using namespace std;
using namespace IR;

void MipsBuilder::build() {
  build_data();
  build_text();
}

void MipsBuilder::build_data() {
  map<string, bool> allocated;
  data_insns.push_back(make_shared<MipsInstruction>("    .data"));

  for (int line = 0; line < ir_insns.size(); ++line) {
    auto insn = ir_insns[line];
    vector<SymbolInfoPtr> to_check = insn->get_reads();
    SymbolInfoPtr write = insn->get_write();
    if (write) {
      to_check.push_back(write);
    }
    for (SymbolInfoPtr symbol : to_check) {
      if (allocated.count(symbol->unique_name)) {
        continue;
      }
      if (symbol->category != SymbolCategory::VARIABLE &&
          symbol->category != SymbolCategory::TEMPORARY) {
        continue;
      }
      allocated[symbol->unique_name] = true;
      data_insns.push_back(make_shared<Label>(symbol->unique_name));
      ostringstream insn;
      if (symbol->flattened_info.is_array) {
        insn << "    .space " << symbol->flattened_info.element_count * 4;
      } else if (symbol->flattened_info.base_type == NumericalType::INT) {
        insn << "    .word 0";
      } else {
        insn << "    .float 0.0";
      }
      data_insns.push_back(make_shared<MipsInstruction>(insn.str()));
    }
  }
}

shared_ptr<MipsRegister> MipsRegister::ZERO(
    make_shared<MipsRegister>(NumericalType::INT, 0));
shared_ptr<MipsRegister> MipsRegister::V0(
    make_shared<MipsRegister>(NumericalType::INT, 2));
shared_ptr<MipsRegister> MipsRegister::SP(
    make_shared<MipsRegister>(NumericalType::INT, 29));
shared_ptr<MipsRegister> MipsRegister::FP(
    make_shared<MipsRegister>(NumericalType::INT, 30));
shared_ptr<MipsRegister> MipsRegister::RA(
    make_shared<MipsRegister>(NumericalType::INT, 31));

#define IF_DYNAMIC(type, obj) \
  if (shared_ptr<type> typed = dynamic_pointer_cast<type>(obj))

struct Builtins {
  Builtins() {
    builtins["printi"] = R"(
lw $a0, 0($sp)
li $v0, 1 # print_int syscall
syscall
jr $ra
)";
    builtins["flush"] = R"(
jr $ra # doing nothing
)";
    builtins["not"] = R"(
lw $v0, 0($sp)
seq $v0, $v0, $zero
jr $ra
)";
  }
  map<string, string> builtins;
};

Builtins builtins;

shared_ptr<MipsSymbol> MipsBuilder::alloc_to_mips_symbol(
    SymbolInfoPtr symbol, Allocation allocation) const {
  shared_ptr<MipsSymbol> ret;

  //std::cout << "Allocation type: " << static_cast<int>(allocation.type) << " " << "\n";

  switch (allocation.type) {
    case Allocation::Type::MEMORY:
      ret = make_shared<MipsLabel>(symbol->unique_name);
      break;
    case Allocation::Type::ARGUMENT:
      ret = make_shared<MipsRelativeAddress>(MipsRegister::FP,
                                             allocation.argument_number * 4);
      break;
    case Allocation::Type::LOCAL_VARIABLE:
      ret = make_shared<MipsRelativeAddress>(MipsRegister::FP,
                                             -(allocation.slot_number + 2) * 4);
      throw std::runtime_error("Local variable is not supported");
    case Allocation::Type::REGISTER:
      ret = make_shared<MipsRegister>(symbol->flattened_info.base_type,
                                      allocation.register_number);
      break;
    default:
      throw std::runtime_error("store_to_symbol: Not implemented");
  }
  return ret;
}

void MipsBuilder::spill_fill(ExtraAction action) {
  if (action.type == ExtraAction::Action::SPILL) {
    // Only support one spill method: store to memory.
    shared_ptr<MipsRegister> source = make_shared<MipsRegister>(
        action.symbol->flattened_info.base_type, action.register_number);
    shared_ptr<MipsLabel> dest =
        make_shared<MipsLabel>(action.symbol->unique_name);
    store_to_address(source, dest);
  } else if (action.type == ExtraAction::Action::FILL) {
    shared_ptr<MipsRegister> dest = make_shared<MipsRegister>(
        action.symbol->flattened_info.base_type, action.register_number);
    shared_ptr<MipsSymbol> source;
    if (action.symbol->category == SymbolCategory::VARIABLE ||
        action.symbol->category == SymbolCategory::TEMPORARY) {
      source = make_shared<MipsLabel>(action.symbol->unique_name);
    } else if (action.symbol->category == SymbolCategory::ARGUMENT) {
      source = make_shared<MipsRelativeAddress>(
          MipsRegister::FP, 4 * action.symbol->argument_position);
    }
    load_to_register(source, dest);
  } else {
    throw out_of_range("Unknown pre/post allocation action");
  }
}

void MipsBuilder::build_text() {
  text_insns.push_back(make_shared<MipsInstruction>("    .text"));
  text_insns.push_back(make_shared<MipsInstruction>("    .globl main"));
  set<string> used_builtins;

  for (int line = 0; line < ir_insns.size(); ++line) {
    auto  insn       = ir_insns[line];

    shared_ptr<ExtraActionList> preactions = alloc_map.get_preactions(line);
    for (auto action : *preactions) {
      spill_fill(action);
    }
    IF_DYNAMIC(AssignInstruction, insn) {
      translate(typed, line);
    }
    else IF_DYNAMIC(ArrayOpInstruction, insn) {
      translate(typed, line);
    }
    else IF_DYNAMIC(GotoInstruction, insn) {
      translate(typed, line);
    }
    else IF_DYNAMIC(Label, insn) {
      text_insns.push_back(typed);
    }
    else IF_DYNAMIC(BranchInstruction, insn) {
      translate(typed, line);
    }
    else IF_DYNAMIC(ReturnInstruction, insn) {
      translate(typed, line);
    }
    else IF_DYNAMIC(BinOpInstruction, insn) {
      translate(typed, line);
    }
    else IF_DYNAMIC(CallInstruction, insn) {
      translate(typed, line);
      if (typed->func->scope == Scope::GLOBAL) {
        string name = typed->func->local_name;
        used_builtins.insert(name);
      }
    }
    else IF_DYNAMIC(CreateProlog, insn) {
      translate(typed, line);
    }
    shared_ptr<ExtraActionList> postactions = alloc_map.get_postactions(line);
    for (auto action : *postactions) {
      spill_fill(action);
    }
  }
  for (string func_name : used_builtins) {
    string label = generate_unique_name(Scope::GLOBAL, func_name);
    text_insns.push_back(make_shared<Label>(label));
    text_insns.push_back(
        make_shared<MipsInstruction>(builtins.builtins.at(func_name)));
  }
}

void MipsBuilder::load_to_register(shared_ptr<MipsSymbol> source,
                                   shared_ptr<MipsRegister> dest) {
  IF_DYNAMIC(MipsImmediate, source) {
    if (typed->type == NumericalType::FLOAT) {
      set_literal(dest, typed->float_value);
    } else {
      set_literal(dest, typed->int_value);
    }
  }
  else IF_DYNAMIC(MipsAddress, source) {
    string op;
    if (dest->type == NumericalType::INT) {
      op = "lw";
    } else {
      op = "lwc1";
    }
    text_insns.push_back(make_shared<MipsInstruction>(op, dest, source));
  }
  else IF_DYNAMIC(MipsRegister, source) {
    if (typed->type == dest->type) {
      if (typed->number == dest->number) {
        // same source and dest, ignore
        return;
      }
      text_insns.push_back(make_shared<MipsInstruction>("move", dest, typed));
    } else {
      if (typed->type == NumericalType::INT) {
        text_insns.push_back(make_shared<MipsInstruction>("mtc1", typed, dest));
        text_insns.push_back(
            make_shared<MipsInstruction>("cvt.w.s", dest, dest));
      } else {
        text_insns.push_back(make_shared<MipsInstruction>("mfc1", dest, typed));
        text_insns.push_back(
            make_shared<MipsInstruction>("cvt.s.w", dest, dest));
      }
    }
  }
}

shared_ptr<MipsRegister> MipsBuilder::load_from_symbol(SymbolInfoPtr symbol,
                                                       int line,
                                                       bool* need_release) {
  *need_release = false;
  shared_ptr<MipsSymbol> source;
  shared_ptr<MipsRegister> ret;
  if (symbol->category == SymbolCategory::LITERAL) {
    // TODO: Literal needs register allocation too.
    ret = acquire_temp_register(symbol->flattened_info.base_type);
    *need_release = true;
    if (symbol->flattened_info.base_type == NumericalType::INT) {
      source = make_shared<MipsImmediate>(symbol->int_value);
    } else {
      source = make_shared<MipsImmediate>(symbol->float_value);
    }
    load_to_register(source, ret);
    return ret;
  }

  Allocation allocation = alloc_map.get(symbol, line, AccessType::READ);
  if (allocation.type == Allocation::Type::MEMORY ||
      allocation.type == Allocation::Type::ARGUMENT ||
      allocation.type == Allocation::Type::LOCAL_VARIABLE) {
    ret = acquire_temp_register(symbol->flattened_info.base_type);
    *need_release = true;
    shared_ptr<MipsAddress> source;
    if (allocation.type == Allocation::Type::ARGUMENT) {
      source = make_shared<MipsRelativeAddress>(MipsRegister::FP,
                                                4 * allocation.slot_number);
    } else {
      source = make_shared<MipsLabel>(symbol->unique_name);
    }
    load_to_register(source, ret);
    return ret;
  } else if (allocation.type == Allocation::Type::REGISTER) {
    // The symbol value is already in the register.
    ret = make_shared<MipsRegister>(symbol->flattened_info.base_type,
                                    allocation.register_number);
    return ret;
  }
  throw std::invalid_argument("load_from_symbol");
}

void MipsBuilder::store_to_address(shared_ptr<MipsRegister> source,
                                   shared_ptr<MipsAddress> dest) {
  string op;
  if (source->type == NumericalType::FLOAT) {
    op = "s.s";
  } else {
    op = "sw";
  }
  text_insns.push_back(make_shared<MipsInstruction>(op, source, dest));
}

void MipsBuilder::store_to_symbol(shared_ptr<MipsRegister> source,
                                  SymbolInfoPtr symbol, int line) {
  Allocation allocation = alloc_map.get(symbol, line, AccessType::WRITE);
  shared_ptr<MipsSymbol> dest = alloc_to_mips_symbol(symbol, allocation);
  IF_DYNAMIC(MipsAddress, dest) { store_to_address(source, typed); }
  else IF_DYNAMIC(MipsRegister, dest) {
    load_to_register(source, typed);
  }
}

string MipsBuilder::new_label(string name) {
  int i = ++label_counters[name];
  ostringstream ss;
  ss << name << '_' << i;
  return ss.str();
}

void MipsBuilder::translate(shared_ptr<AssignInstruction> ir, int line) {
  SymbolInfoPtr ir_dest = ir->dest;
  bool need_release;
  shared_ptr<MipsRegister> source =
      load_from_symbol(ir->value, line, &need_release);
  if (ir_dest->flattened_info.base_type != source->type) {
    // need type conversion
    if (source->type == NumericalType::INT) {
      // int --> float
      shared_ptr<MipsRegister> temp =
          acquire_temp_register(NumericalType::FLOAT);
      text_insns.push_back(make_shared<MipsInstruction>("mtc1", source, temp));
      text_insns.push_back(make_shared<MipsInstruction>("cvt.s.w", temp, temp));
      if (need_release) {
        release_register(source);
      }
      source = temp;
      need_release = true;
    } else {
      // float --> int
      shared_ptr<MipsRegister> temp =
          acquire_temp_register(NumericalType::INT);
      text_insns.push_back(
          make_shared<MipsInstruction>("cvt.w.s", source, source));
      text_insns.push_back(make_shared<MipsInstruction>("mfc1", temp, source));
      if (need_release) {
        release_register(source);
      }
      source = temp;
      need_release = true;
    }
  }
  if (ir->size < 0) {
    store_to_symbol(source, ir_dest, line);
  } else if (ir->size > 0) {
    shared_ptr<MipsRegister> pointer =
        acquire_temp_register(NumericalType::INT);
    text_insns.push_back(make_shared<MipsInstruction>(
        "la", pointer, make_shared<MipsLabel>(ir_dest->unique_name)));
    shared_ptr<MipsRegister> counter =
        acquire_temp_register(NumericalType::INT);
    set_literal(counter, ir->size);
    string label = new_label("assign");
    text_insns.push_back(make_shared<Label>(label));
    shared_ptr<MipsRelativeAddress> dest =
        make_shared<MipsRelativeAddress>(pointer, 0);
    store_to_address(source, dest);
    text_insns.push_back(make_shared<MipsInstruction>(
        "add", pointer, pointer, make_shared<MipsImmediate>(4)));
    text_insns.push_back(make_shared<MipsInstruction>(
        "sub", counter, counter, make_shared<MipsImmediate>(1)));
    text_insns.push_back(make_shared<MipsInstruction>(
        "bgt", counter, MipsRegister::ZERO, make_shared<MipsLabel>(label)));
    release_register(counter);
    release_register(pointer);
  }
  if (need_release) {
    release_register(source);
  }
}

void MipsBuilder::translate(shared_ptr<GotoInstruction> insn, int line) {
  text_insns.push_back(
      make_shared<MipsInstruction>("b", make_shared<MipsLabel>(insn->label)));
}

void MipsBuilder::translate(shared_ptr<ReturnInstruction> insn, int line) {
  SymbolInfoPtr ir_source = insn->value;
  if (ir_source != SymbolInfo::NOT_FOUND) {
    bool need_release;
    shared_ptr<MipsRegister> source =
        load_from_symbol(ir_source, line, &need_release);
    text_insns.push_back(
        make_shared<MipsInstruction>("move", MipsRegister::V0, source));
    if (need_release) {
      release_register(source);
    }
  }
  // Here is the epilogue
  load_to_register(make_shared<MipsRelativeAddress>(MipsRegister::SP, 0),
                   MipsRegister::FP);
  load_to_register(make_shared<MipsRelativeAddress>(MipsRegister::SP, 4),
                   MipsRegister::RA);
  text_insns.push_back(
      make_shared<MipsInstruction>("add", MipsRegister::SP, MipsRegister::SP,
                                   make_shared<MipsImmediate>(8)));
  text_insns.push_back(make_shared<MipsInstruction>("jr", MipsRegister::RA));
}

void MipsBuilder::translate(shared_ptr<BranchInstruction> insn, int line) {
  SymbolInfoPtr ir_arg1 = insn->arg1;
  SymbolInfoPtr ir_arg2 = insn->arg2;
  bool release_1 = false;
  bool release_2 = false;
  shared_ptr<MipsRegister> arg1 = load_from_symbol(ir_arg1, line, &release_1);
  shared_ptr<MipsRegister> arg2 = load_from_symbol(ir_arg2, line, &release_2);
  shared_ptr<MipsSymbol> label(make_shared<MipsLabel>(insn->label));
  // The IR guarantees that both arguments are of the same type.
  if (ir_arg1->flattened_info.base_type == NumericalType::INT) {
    string op;
    if (insn->op == "breq") {
      op = "beq";
    } else if (insn->op == "brneq") {
      op = "bne";
    } else if (insn->op == "brlt") {
      op = "blt";
    } else if (insn->op == "brleq") {
      op = "ble";
    } else if (insn->op == "brgt") {
      op = "bgt";
    } else if (insn->op == "brgeq") {
      op = "bge";
    }
    text_insns.push_back(make_shared<MipsInstruction>(op, arg1, arg2, label));
  } else {
    string compare_op;
    string branch_op;
    if (insn->op == "breq") {
      compare_op = "c.e.s";
      branch_op = "bc1t";
    } else if (insn->op == "brneq") {
      compare_op = "c.e.s";
      branch_op = "bc1f";
    } else if (insn->op == "brlt") {
      compare_op = "c.lt.s";
      branch_op = "bc1t";
    } else if (insn->op == "brleq") {
      compare_op = "c.le.s";
      branch_op = "bc1t";
    } else if (insn->op == "brgt") {
      compare_op = "c.le.s";
      branch_op = "bc1f";
    } else if (insn->op == "brgeq") {
      compare_op = "c.lt.s";
      branch_op = "bc1f";
    }
    text_insns.push_back(make_shared<MipsInstruction>(compare_op, arg1, arg2));
    text_insns.push_back(make_shared<MipsInstruction>(branch_op, label));
  }
  if (release_1) {
    release_register(arg1);
  }
  if (release_2) {
    release_register(arg2);
  }
}

void MipsBuilder::translate(std::shared_ptr<BinOpInstruction> ir_insn,
                            int line) {
  SymbolInfoPtr ir_arg1 = ir_insn->arg1;
  SymbolInfoPtr ir_arg2 = ir_insn->arg2;
  SymbolInfoPtr ir_dest = ir_insn->dest;
  bool release_1 = false;
  bool release_2 = false;
  bool release_dest = false;
  shared_ptr<MipsRegister> arg1 = load_from_symbol(ir_arg1, line, &release_1);
  shared_ptr<MipsRegister> arg2 = load_from_symbol(ir_arg2, line, &release_2);
  // TODO don't always acquire register
  shared_ptr<MipsRegister> dest =
      acquire_temp_register(ir_dest->flattened_info.base_type);
  release_dest = true;
  string op;
  if (ir_dest->flattened_info.base_type == NumericalType::FLOAT) {
    if (ir_insn->op == "add") {
      op = "add.s";
    } else if (ir_insn->op == "sub") {
      op = "sub.s";
    } else if (ir_insn->op == "mult") {
      op = "mul.s";
    } else if (ir_insn->op == "div") {
      op = "div.s";
    } else {
      throw std::invalid_argument("Unknown floating operator: " + ir_insn->op);
    }
  } else {
    if (ir_insn->op == "or") {
      op = "or";
    } else if (ir_insn->op == "and") {
      op = "and";
    } else if (ir_insn->op == "add") {
      op = "add";
    } else if (ir_insn->op == "sub") {
      op = "sub";
    } else if (ir_insn->op == "mult") {
      op = "mul";
    } else if (ir_insn->op == "div") {
      op = "div";
    } else {
      throw std::invalid_argument("Unknown integer operator: " + ir_insn->op);
    }
  }
  if (op == "mult" || op == "div") {
    // These instructions only take two registers.
    text_insns.push_back(make_shared<MipsInstruction>(op, arg1, arg2));
    text_insns.push_back(make_shared<MipsInstruction>("mflo", dest));
  } else {
    text_insns.push_back(make_shared<MipsInstruction>(op, dest, arg1, arg2));
  }
  if (release_1) {
    release_register(arg1);
  }
  if (release_2) {
    release_register(arg2);
  }
  // TODO may not need store
  store_to_symbol(dest, ir_dest, line);
  if (release_dest) {
    release_register(dest);
  }
}

void MipsBuilder::translate(shared_ptr<CallInstruction> ir_insn, int line) {
  int frame_size = ir_insn->args.size() * 4;
  frame_size = ((frame_size + 7) / 8) * 8;  // align to 8-byte boundary
  text_insns.push_back(
      make_shared<MipsInstruction>("sub", MipsRegister::SP, MipsRegister::SP,
                                   make_shared<MipsImmediate>(frame_size)));
  for (int i = 0; i < ir_insn->args.size(); ++i) {
    SymbolInfoPtr ir_arg = ir_insn->args[i];
    bool need_release = false;
    shared_ptr<MipsRegister> r = load_from_symbol(ir_arg, line, &need_release);
    int stack_offset = i * 4;
    shared_ptr<MipsRelativeAddress> slot(
        new MipsRelativeAddress(MipsRegister::SP, stack_offset));
    store_to_address(r, slot);
    if (need_release) {
      release_register(r);
    }
  }
  text_insns.push_back(make_shared<MipsInstruction>(
      "jal", make_shared<MipsLabel>(ir_insn->func->unique_name)));
  text_insns.push_back(
      make_shared<MipsInstruction>("add", MipsRegister::SP, MipsRegister::SP,
                                   make_shared<MipsImmediate>(frame_size)));
  if (ir_insn->can_write()) {
    store_to_symbol(MipsRegister::V0, ir_insn->dest, line);
  }
}

void MipsBuilder::translate(std::shared_ptr<CreateProlog> ir_insn, int line) {
  store_to_address(MipsRegister::RA,
                   make_shared<MipsRelativeAddress>(MipsRegister::SP, -4));
  store_to_address(MipsRegister::FP,
                   make_shared<MipsRelativeAddress>(MipsRegister::SP, -8));
  text_insns.push_back(
      make_shared<MipsInstruction>("move", MipsRegister::FP, MipsRegister::SP));
  text_insns.push_back(
      make_shared<MipsInstruction>("sub", MipsRegister::SP, MipsRegister::SP,
                                   make_shared<MipsImmediate>(8)));
}

void MipsBuilder::set_literal(std::shared_ptr<MipsRegister> r, int value) {
  if (value == 0) {
    text_insns.push_back(make_shared<MipsInstruction>(
        "xor", r, MipsRegister::ZERO, MipsRegister::ZERO));
  } else {
    shared_ptr<MipsImmediate> imm(make_shared<MipsImmediate>(value));
    text_insns.push_back(make_shared<MipsInstruction>("li", r, imm));
  }
}

void MipsBuilder::set_literal(std::shared_ptr<MipsRegister> r, float value) {
  if (value == 0) {
    text_insns.push_back(
        make_shared<MipsInstruction>("mtc1", MipsRegister::ZERO, r));
  } else {
    shared_ptr<MipsImmediate> imm(make_shared<MipsImmediate>(value));
    text_insns.push_back(make_shared<MipsInstruction>("li.s", r, imm));
  }
}

shared_ptr<MipsRegister> MipsBuilder::acquire_temp_register(NumericalType type) {
  vector<bool>* free_list;
  if (type == NumericalType::INT) {
    free_list = &free_int_registers;
  } else {
    free_list = &free_float_registers;
  }
  int limit = register_limit + feasible_count;
  for (int i = register_limit; i < limit; ++i) {
    if (free_list->at(i)) {
      free_list->at(i) = false;
      return make_shared<MipsRegister>(type, i);
    }
  }
  // TODO: No register available, need to free up some.
  // Probably responsibility of the register allocator though.
  throw runtime_error("No more free register");
}

void MipsBuilder::release_register(shared_ptr<MipsRegister> r) {
  vector<bool>* free_list;
  if (r->type == NumericalType::INT) {
    free_list = &free_int_registers;
  } else {
    free_list = &free_float_registers;
  }
  free_list->at(r->number) = true;
}

void MipsBuilder::translate(shared_ptr<ArrayOpInstruction> ir_insn, int line) {
  SymbolInfoPtr ir_array = ir_insn->array;
  SymbolInfoPtr ir_index = ir_insn->index;
  SymbolInfoPtr ir_value = ir_insn->value;
  // Taking advantage array is always a variable.
  shared_ptr<MipsRegister> pointer = acquire_temp_register(NumericalType::INT);
  text_insns.push_back(make_shared<MipsInstruction>(
      "la", pointer, make_shared<MipsLabel>(ir_array->unique_name)));
  bool need_release;
  shared_ptr<MipsRegister> temp_register =
      load_from_symbol(ir_index, line, &need_release);
  text_insns.push_back(make_shared<MipsInstruction>(
      "mul", temp_register, temp_register, make_shared<MipsImmediate>(4)));
  text_insns.push_back(
      make_shared<MipsInstruction>("add", pointer, pointer, temp_register));
  if (need_release) {
    release_register(temp_register);
  }
  if (ir_insn->op == "array_store") {
    temp_register = load_from_symbol(ir_value, line, &need_release);
    shared_ptr<MipsRelativeAddress> dest =
        make_shared<MipsRelativeAddress>(pointer, 0);
    store_to_address(temp_register, dest);
    if (need_release) {
      release_register(temp_register);
    }
  } else {
    // array_load
    string op;
    shared_ptr<MipsRelativeAddress> source =
        make_shared<MipsRelativeAddress>(pointer, 0);
    Allocation alloc = alloc_map.get(ir_value, line, AccessType::WRITE);
    shared_ptr<MipsSymbol> dest_symbol = alloc_to_mips_symbol(ir_value, alloc);
    IF_DYNAMIC(MipsRegister, dest_symbol) {
      // There is an allocated register, use it
      load_to_register(source, typed);
    }
    else IF_DYNAMIC(MipsAddress, dest_symbol) {
      // No allocated register, grab a temp one, then store back to mem
      shared_ptr<MipsRegister> temp =
          acquire_temp_register(ir_array->flattened_info.base_type);
      load_to_register(source, temp);
      store_to_address(temp, typed);
      release_register(temp);
    }
  }
  release_register(pointer);
}
