#pragma once
#include <cassert>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>

#include "../Parser/SyntaxTree.hpp"

constexpr char GLOBAL_SCOPE_STR[] = "@";
constexpr char MAIN_SCOPE_STR[] = "main";

class Scope {
 public:
  Scope() : path("") {}
  Scope(std::string s) : path(s) {}
  Scope enter(std::string name) const;
  Scope exit() const;
  bool is_empty() const { return path.empty(); }
  std::size_t hash() const {
    std::hash<std::string> hasher;
    return hasher(path);
  }
  std::string get_local() const;
  std::string get_unique() const;
  bool operator==(const Scope& other) const { return path == other.path; }
  bool operator!=(const Scope& other) const { return path != other.path; }
  friend std::ostream& operator<<(std::ostream& out, const Scope& scope) {
    out << scope.path;
    return out;
  }

  static Scope GLOBAL;
  static Scope MAIN;

 private:
  std::string path;
};

namespace std {
template <>
struct hash<Scope> {
  std::size_t operator()(const Scope& scope) const { return scope.hash(); }
};
}

enum class NumericalType { UNKNOWN, INT, FLOAT };

enum class SymbolCategory {
  LITERAL,
  TEMPORARY,
  VARIABLE,
  TYPE,
  FUNCTION,
  ARGUMENT,
};

struct FlattenedTypeInfo {
  NumericalType base_type;
  int element_count;
  bool is_array = false;
};

struct SymbolInfo {
  SymbolCategory category;
  Scope scope;
  std::string local_name;
  std::string unique_name;
  TypeInfoNode declared_info;
  FlattenedTypeInfo flattened_info;
  bool has_type = true;                  // Function may not have return type,
  std::vector<std::string> param_names;  // but may have arguments.
  union {
    int int_value;
    float float_value;
  };
  int argument_position = -1;
  void print(std::ostream& out) const;
  bool operator==(const SymbolInfo& other) const {
    return category == other.category && scope == other.scope &&
           local_name == other.local_name;
  }
  friend std::ostream& operator<<(std::ostream& out, const SymbolInfo& symbol) {
    out << symbol.local_name;
    /* out << '(';
    out << (symbol.flattened_info.base_type == NumericalType::INT ? "int" : "float");
    out << ')'; */
    return out;
  }

  static std::shared_ptr<SymbolInfo> NOT_FOUND;
};

using SymbolInfoPtr = std::shared_ptr<SymbolInfo>;

class ScopedSymbolTable {
 public:
  using Iterator =
      std::unordered_map<std::string, SymbolInfoPtr>::const_iterator;
  ScopedSymbolTable(Scope scope) : scope(scope) {}
  SymbolInfoPtr lookup(std::string name) const;
  void add(SymbolInfoPtr symbol);
  void print(std::ostream& out);
  std::vector<SymbolInfoPtr> get_all_symbols() const;

 private:
  Scope scope;
  std::unordered_map<std::string, SymbolInfoPtr> symbols;
};

class SymbolTable {
 public:
  using Iterator =
      std::unordered_map<Scope,
                         std::shared_ptr<ScopedSymbolTable>>::const_iterator;
  virtual ~SymbolTable() = default;
  SymbolInfoPtr lookup(Scope scope, std::string name) const;
  SymbolInfoPtr scoped_lookup(Scope scope, std::string name) const;
  std::vector<Scope> get_all_scopes() const;
  std::shared_ptr<ScopedSymbolTable> get_scoped_table(Scope s) const;
  void add(Scope scope, SymbolInfoPtr symbol);
  void print(std::ostream& out);

 private:
  std::unordered_map<Scope, std::shared_ptr<ScopedSymbolTable>> scoped_tables;
};

class SymtabBuilder : public AstVisitor {
 public:
  virtual ~SymtabBuilder() = default;
  SymtabBuilder() : symtab(new SymbolTable) {}
  virtual void visit(ProgramNode& node);
  virtual void visit(LetNode& node);
  virtual void visit(FuncDeclNode& node);
  SymbolTable& get_symtab() { return *symtab.get(); }

 private:
  void enter_scope(std::string name) {
    assert(!name.empty());
    scope_counters.push(0);
    current_scope = current_scope.enter(name);
  }
  void exit_scope() {
    current_scope = current_scope.exit();
    scope_counters.pop();
  }
  FlattenedTypeInfo flatten_type(Scope scope, const TypeInfoNode& node) const;
  std::unique_ptr<SymbolTable> symtab;
  Scope current_scope;
  std::stack<int> scope_counters;
};

std::string generate_unique_name(Scope scope, std::string name);
