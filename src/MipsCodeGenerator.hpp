#pragma once
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "IrCodeGenerator.hpp"
#include "Parser/SyntaxTree.hpp"
#include "RegisterAllocator.hpp"
#include "SemanticAnalyzer/SymbolTable.hpp"

struct MipsSymbol {
  virtual ~MipsSymbol() = default;
  virtual void print(std::ostream& out) const {};
  friend std::ostream& operator<<(std::ostream& out, const MipsSymbol& symbol) {
    symbol.print(out);
    return out;
  }
};

constexpr const char* INT_REGISTER_NAMES[32] = {
    "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3", "t0", "t1", "t2",
    "t3",   "t4", "t5", "t6", "t7", "s0", "s1", "s2", "s3", "s4", "s5",
    "s6",   "s7", "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra"};

struct MipsRegister : public MipsSymbol {
  MipsRegister(NumericalType type, int number) : type(type), number(number) {}
  NumericalType type;
  int number;
  virtual void print(std::ostream& out) const {
    if (type == NumericalType::INT) {
      out << "$" << INT_REGISTER_NAMES[number];
    } else {
      out << "$f" << number;
    }
  }
  static std::shared_ptr<MipsRegister> ZERO, V0, SP, FP, RA;
};

struct MipsImmediate : public MipsSymbol {
  MipsImmediate(int value) : int_value(value), type(NumericalType::INT) {}
  MipsImmediate(float value) : float_value(value), type(NumericalType::FLOAT) {}
  NumericalType type;
  union {
    int int_value;
    float float_value;
  };
  virtual void print(std::ostream& out) const {
    if (type == NumericalType::INT) {
      out << int_value;
    } else {
      std::ostringstream ss;
      ss << float_value;
      if (ss.str().find('.') == std::string::npos) {
        ss << ".0";
      }
      out << ss.str();
    }
  };
};

struct MipsAddress : public MipsSymbol {};
struct MipsLabel   : public MipsAddress {
  MipsLabel(std::string label) : label(label) {}
  std::string label;
  virtual void print(std::ostream& out) const { out << label; };
};

struct MipsRelativeAddress : public MipsAddress {
  MipsRelativeAddress(std::shared_ptr<MipsRegister> r, int offset)
      : base_register(r), offset(offset) {}
  std::shared_ptr<MipsRegister> base_register;
  int offset;
  virtual void print(std::ostream& out) const {
    out << offset << '(' << *base_register << ')';
  };
};

class MipsInstruction : public Instruction {
 public:
  template <typename... T>
  MipsInstruction(std::string op, T... args) : op(op), args({args...}) {}
  virtual void print(std::ostream& out) const override {
    out << "    " << op;
    for (int i = 0; i < args.size(); ++i) {
      if (i > 0) {
        out << ',';
      }
      out << ' ' << *args[i];
    }
  }

 private:
  std::string op;
  std::vector<std::shared_ptr<MipsSymbol>> args;
};

class MipsBuilder {
 public:

  MipsBuilder(
    const InstructionList&  insns,
    const SymbolTable&      symtab,
    const AllocationMap&    alloc_map)
      : ir_insns(insns), symtab(symtab), alloc_map(alloc_map) {

    free_int_registers.assign  (32, false);
    free_float_registers.assign(32, false);
    for (int i = register_limit; i < register_limit + feasible_count; ++i) {
      free_int_registers[i]   = true;
      free_float_registers[i] = true;
    }
  }
  void build();
  const InstructionList& get_data() const { return data_insns; }
  const InstructionList& get_text() const { return text_insns; }

 private:
  static constexpr int feasible_count = 4;  // can use up to this many registers
  static constexpr int register_limit = 24;  // use registers starting from here

  void build_data();
  void build_text();
  void translate(std::shared_ptr<IR::AssignInstruction>  insn, int line);
  void translate(std::shared_ptr<IR::GotoInstruction>    insn, int line);
  void translate(std::shared_ptr<IR::BranchInstruction>  insn, int line);
  void translate(std::shared_ptr<IR::ReturnInstruction>  insn, int line);
  void translate(std::shared_ptr<IR::BinOpInstruction>   insn, int line);
  void translate(std::shared_ptr<IR::CallInstruction>    insn, int line);
  void translate(std::shared_ptr<CreateProlog>           insn, int line);
  void translate(std::shared_ptr<IR::ArrayOpInstruction> insn, int line);

  void set_literal(std::shared_ptr<MipsRegister> r, int   value);
  void set_literal(std::shared_ptr<MipsRegister> r, float value);

  std::shared_ptr<MipsRegister> acquire_temp_register(NumericalType type);

  void release_register(std::shared_ptr<MipsRegister> r);

  std::shared_ptr<MipsSymbol> alloc_to_mips_symbol(
    SymbolInfoPtr symbol,
    Allocation    alloc) const;

  // Loads symbol source to a register dest.
  void load_to_register(
    std::shared_ptr<MipsSymbol>   source,
    std::shared_ptr<MipsRegister> dest);

  std::shared_ptr<MipsRegister> load_from_symbol(
    SymbolInfoPtr symbol,
    int           line,
    bool*         need_release);

  // Stores source to a memory location by dest.
  void store_to_address(
    std::shared_ptr<MipsRegister> source,
    std::shared_ptr<MipsAddress>  dest);

  void store_to_symbol(
    std::shared_ptr<MipsRegister> source,
    SymbolInfoPtr                 dest,
    int                           line);

  void spill_fill(ExtraAction action);
  std::string new_label(std::string name);
  std::map<std::string, int> label_counters;

  std::vector<bool> free_int_registers;
  std::vector<bool> free_float_registers;

  const InstructionList&  ir_insns;
  const SymbolTable&      symtab;
  const AllocationMap&    alloc_map;

  InstructionList data_insns;
  InstructionList text_insns;
};
