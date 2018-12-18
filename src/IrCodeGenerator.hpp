#pragma once
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "Parser/SyntaxTree.hpp"
#include "SemanticAnalyzer/SymbolTable.hpp"

class Instruction {
 public:
  virtual ~Instruction() = default;
  virtual bool can_write() const {
    return get_write() != SymbolInfo::NOT_FOUND;
  }
  virtual SymbolInfoPtr get_write() const {  // Single write.
    return SymbolInfo::NOT_FOUND;
  };
  virtual std::vector<SymbolInfoPtr> get_reads() const {  // Many reads.
    return {};
  }
  virtual void print(std::ostream& out) const {};
  friend std::ostream& operator<<(std::ostream& out, const Instruction& insn) {
    insn.print(out);
    return out;
  }
  virtual int topology() {
    return -1;
  }
  virtual std::string get_label() {
    return "";
  }

  NumericalType numerical_type = NumericalType::UNKNOWN;
};

using InstructionList = std::vector<std::shared_ptr<Instruction>>;
using InstructionMap  = std::map<std::shared_ptr<Instruction>,int>;

class TopologyMutatorInstruction : public Instruction {
public:
    std::string get_label() override {
      return label;
    }
    std::string label;
};


class Comment : public Instruction {
 public:
  Comment(std::string text) : text(text) {}
  virtual void print(std::ostream& out) const override { out << "# " << text; }

  std::string text;
};

class Directive : public Comment {
 public:
  Directive(std::string text) : Comment(text) {}
};

class EnterScope : public Directive {
 public:
  EnterScope(std::string local_name)
      : local_name(local_name), Directive("enter scope " + local_name) {}

  std::string local_name;
};

class ExitScope : public Directive {
 public:
  ExitScope() : Directive("exit scope") {}
};

class CreateProlog : public Directive {
 public:
  CreateProlog() : Directive("placeholder for the prologue") {}
};

class Label : public TopologyMutatorInstruction {
 public:
  Label(std::string label){this -> label = label;}
  virtual void print(std::ostream& out) const override { out << label << ":"; }

  int topology() override {
    return 0;
  }
};

namespace IR {

class AssignInstruction : public Instruction {
 public:
  AssignInstruction(SymbolInfoPtr dest, SymbolInfoPtr value)
      : dest(dest), value(value), size(-1) {}
  AssignInstruction(SymbolInfoPtr dest, int size, SymbolInfoPtr value)
      : dest(dest), value(value), size(size) {}
  virtual SymbolInfoPtr get_write() const override { return dest; }
  virtual std::vector<SymbolInfoPtr> get_reads() const override {
    if (value->category == SymbolCategory::LITERAL) {
      return {};
    }
    return {value};
  }
  virtual void print(std::ostream& out) const override {
    out << "    assign, " << *dest << ", ";
    if (size < 0) {
      out << *value << ",";
    } else {
      out << size << ", " << *value;
    }
  }

  SymbolInfoPtr dest, value;
  int size;
};

class BinOpInstruction : public Instruction {
 public:
  BinOpInstruction(std::string op, SymbolInfoPtr dest, SymbolInfoPtr arg1,
                   SymbolInfoPtr arg2)
      : op(op), dest(dest), arg1(arg1), arg2(arg2) {}
  virtual bool can_write() const override { return true; }
  virtual SymbolInfoPtr get_write() const override { return dest; }
  virtual std::vector<SymbolInfoPtr> get_reads() const override {
    return {arg1, arg2};
  }
  virtual void print(std::ostream& out) const override {
    out << "    " << op << ", " << *arg1 << ", " << *arg2 << ", " << *dest;
  }

  std::string op;
  SymbolInfoPtr dest, arg1, arg2;
};

class ArrayOpInstruction : public Instruction {
 public:
  static ArrayOpInstruction read(SymbolInfoPtr array, SymbolInfoPtr index,
                                 SymbolInfoPtr dest) {
    return ArrayOpInstruction("array_load", array, index, dest);
  }
  static ArrayOpInstruction write(SymbolInfoPtr array, SymbolInfoPtr index,
                                  SymbolInfoPtr value) {
    return ArrayOpInstruction("array_store", array, index, value);
  }
  virtual SymbolInfoPtr get_write() const override {
    if (op == "array_load") {
      return value;
    }
    return array;
  }
  virtual std::vector<SymbolInfoPtr> get_reads() const override {
    if (op == "array_store") {
      return {array, index, value};
    }
    return {array, index};
  }
  virtual void print(std::ostream& out) const override {
    if (op == "array_store") {
      out << "    " << op << ", " << *array << ", " << *index << ", " << *value;
    } else {
      out << "    " << op << ", " << *value << ", " << *array << ", " << *index;
    }
  }

  ArrayOpInstruction(std::string op, SymbolInfoPtr array, SymbolInfoPtr index,
                     SymbolInfoPtr value)
      : op(op), array(array), index(index), value(value) {}
  std::string op;
  SymbolInfoPtr array, index, value;
};


class GotoInstruction : public TopologyMutatorInstruction {
 public:
  GotoInstruction(std::string label){this -> label = label;}
  virtual void print(std::ostream& out) const override {
    out << "    goto, " << label << ", ,";
  }
  int topology() override {
    return 1;
  }
};

class BranchInstruction : public TopologyMutatorInstruction {
 public:
  BranchInstruction(std::string op, SymbolInfoPtr arg1, SymbolInfoPtr arg2,
                    std::string label)
      : op(op), arg1(arg1), arg2(arg2) {this -> label = label;}
  virtual std::vector<SymbolInfoPtr> get_reads() const override {
    return {arg1, arg2};
  }
  virtual void print(std::ostream& out) const override {
    out << "    " << op << ", " << *arg1 << ", " << *arg2 << ", " << label;
  }

  int topology() override {
    return 2;
  }

  std::string op;
  SymbolInfoPtr arg1, arg2;
};

class ReturnInstruction : public Instruction {
 public:
  ReturnInstruction() : value(SymbolInfo::NOT_FOUND) {}
  ReturnInstruction(SymbolInfoPtr value) : value(value) {}
  virtual std::vector<SymbolInfoPtr> get_reads() const override {
    if (value != SymbolInfo::NOT_FOUND &&
        value->category == SymbolCategory::LITERAL) {
      return {};
    }
    return {value};
  }
  virtual void print(std::ostream& out) const override {
    out << "    return, ";
    if (value != SymbolInfo::NOT_FOUND) {
      out << *value;
    }
    out << ", ,";
  }

  SymbolInfoPtr value;
};

class CallInstruction : public Instruction {
 public:
  CallInstruction(SymbolInfoPtr func, std::vector<SymbolInfoPtr> args)
      : op("call"), dest(SymbolInfo::NOT_FOUND), func(func), args(args) {}
  CallInstruction(SymbolInfoPtr dest, SymbolInfoPtr func,
                  std::vector<SymbolInfoPtr> args)
      : op("callr"), dest(dest), func(func), args(args) {}
  virtual bool can_write() const override {
    return dest != SymbolInfo::NOT_FOUND;
  }
  virtual SymbolInfoPtr get_write() const override { return dest; };
  virtual std::vector<SymbolInfoPtr> get_reads() const override { return args; }
  virtual void print(std::ostream& out) const override {
    out << "    " << op << ", ";
    if (dest != SymbolInfo::NOT_FOUND) {
      out << *dest << ", ";
    }
    out << *func;
    for (auto arg : args) {
      out << ", " << *arg;
    }
  }

  std::string op;
  SymbolInfoPtr dest, func;
  std::vector<SymbolInfoPtr> args;
};

}  // namespace IR

class IrBuilder : public AstVisitor {
 public:
  IrBuilder(SymbolTable& symtab) : symtab(symtab){};
  DECLARE_VISIT_METHODS();
  const InstructionList& get_insns() const { return insns; }

 private:
  void visit(FuncCallNode& node, bool store_result);
  void visit(BinaryExprNode& node, NumericalType coerced_type);
  void visit(ExprNode& node, NumericalType coerced_type);
  // Coerces the symbol at the top of expr_stack into coerced_type.
  void coerce(NumericalType coerced_type);
  void enter_scope(std::string name) {
    insns.push_back(std::make_shared<EnterScope>(name));
    current_scope = current_scope.enter(name);
    scope_counters.push(0);
  }
  void exit_scope() {
    scope_counters.pop();
    current_scope = current_scope.exit();
    insns.push_back(std::make_shared<ExitScope>());
  }
  NumericalType get_type(const ExprNode* node) const;
  Scope current_scope;
  SymbolTable& symtab;
  SymbolInfoPtr new_temp(NumericalType type);
  SymbolInfoPtr new_literal(int value);
  SymbolInfoPtr new_literal(float value);
  std::string new_label(std::string key, bool store_label = false);
  std::string last_label() const;

  std::unordered_map<std::string, int> label_counters;
  std::stack<SymbolInfoPtr> expr_stack;
  std::stack<std::string> label_stack;
  std::stack<int> scope_counters;
  std::vector<std::shared_ptr<Instruction>> insns;
};
