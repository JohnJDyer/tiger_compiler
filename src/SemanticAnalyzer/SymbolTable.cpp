#include "SymbolTable.hpp"

#include <sstream>
#include <string>
#include <vector>

using namespace std;

SymbolInfoPtr SymbolInfo::NOT_FOUND(new SymbolInfo);

Scope Scope::GLOBAL(GLOBAL_SCOPE_STR);
Scope Scope::MAIN(GLOBAL.enter(MAIN_SCOPE_STR));

std::string generate_unique_name(Scope scope, std::string name) {
  if (scope == Scope::GLOBAL && name == MAIN_SCOPE_STR) {
    return name;
  }
  return scope.get_unique() + "_" + name;
}

Scope Scope::enter(string name) const {
  Scope other = *this;
  if (path.empty()) {
    other.path += name;
  } else {
    other.path += "." + name;
  }
  return other;
}

Scope Scope::exit() const {
  if (path.empty()) {
    throw std::runtime_error("Cannot exit empty scope");
  }
  int pos = path.find_last_of('.');
  Scope other = *this;
  if (pos != std::string::npos) {
    other.path = other.path.substr(0, pos);
  } else {
    other.path = "";
  }
  return other;
}

string Scope::get_local() const {
  if (path.empty()) {
    throw std::runtime_error("No local in an empty scope");
  }
  int pos = path.find_last_of('.');
  return path.substr(pos + 1);
}

string Scope::get_unique() const {
  ostringstream name;
  name << "_";
  name << hash();
  return name.str();
}

void ScopedSymbolTable::add(SymbolInfoPtr info) {
  if (info->unique_name.empty()) {
    info->unique_name = generate_unique_name(scope, info->local_name);
  }
  info->scope = scope;
  symbols[info->local_name] = info;
}

void SymbolTable::add(Scope scope, SymbolInfoPtr info) {
  std::shared_ptr<ScopedSymbolTable> scoped_table;
  SymbolTable::Iterator found = scoped_tables.find(scope);
  if (found == scoped_tables.end()) {
    scoped_table =
        std::shared_ptr<ScopedSymbolTable>(new ScopedSymbolTable(scope));
    scoped_tables[scope] = scoped_table;
  } else {
    scoped_table = found->second;
  }
  scoped_table->add(info);
}

void SymtabBuilder::visit(ProgramNode& node) {
  enter_scope(GLOBAL_SCOPE_STR);

  SymbolInfoPtr flush(new SymbolInfo);
  flush->category = SymbolCategory::FUNCTION;
  flush->has_type = false;
  flush->local_name = "flush";
  symtab->add(current_scope, flush);

  SymbolInfoPtr printi(new SymbolInfo);
  printi->category = SymbolCategory::FUNCTION;
  printi->has_type = false;
  printi->local_name = "printi";
  printi->param_names.push_back("i");
  symtab->add(current_scope, printi);
  SymbolInfoPtr printi_i(new SymbolInfo);
  printi_i->category = SymbolCategory::ARGUMENT;
  printi_i->argument_position = 0;
  printi_i->local_name = "i";
  printi_i->has_type = true;
  shared_ptr<StringNode> int_type(new StringNode);
  int_type->value = "int";
  printi_i->declared_info.base_type = int_type;
  printi_i->flattened_info.base_type = NumericalType::INT;
  symtab->add(current_scope.enter("printi"), printi_i);

  SymbolInfoPtr not_(new SymbolInfo);
  not_->category = SymbolCategory::FUNCTION;
  not_->has_type = true;
  not_->local_name = "not";
  not_->flattened_info.base_type = NumericalType::INT;
  not_->declared_info.base_type = int_type;
  not_->param_names.push_back("i");
  symtab->add(current_scope, not_);
  SymbolInfoPtr not_i(new SymbolInfo);
  not_i->category = SymbolCategory::ARGUMENT;
  not_i->argument_position = 0;
  not_i->local_name = "i";
  not_i->has_type = true;
  not_i->declared_info.base_type = int_type;
  not_i->flattened_info.base_type = NumericalType::INT;
  symtab->add(current_scope.enter("not"), not_i);

  SymbolInfoPtr exit(new SymbolInfo);
  exit->category = SymbolCategory::FUNCTION;
  exit->has_type = false;
  exit->local_name = "exit";
  symtab->add(current_scope, exit);

  SymbolInfoPtr int_(new SymbolInfo);
  int_->category = SymbolCategory::TYPE;
  int_->has_type = true;
  int_->local_name = "int";
  int_->flattened_info.base_type = NumericalType::INT;
  int_->declared_info.base_type = int_type;
  symtab->add(current_scope, int_);

  SymbolInfoPtr float_(new SymbolInfo);
  float_->category = SymbolCategory::TYPE;
  float_->has_type = true;
  float_->local_name = "float";
  float_->flattened_info.base_type = NumericalType::FLOAT;
  shared_ptr<StringNode> float_type(new StringNode);
  float_type->value = "float";
  float_->declared_info.base_type = float_type;
  symtab->add(current_scope, float_);

  node.LetNode::accept(*this);
  exit_scope();
}

void SymtabBuilder::visit(LetNode& node) {
  if (scope_counters.size() == 1) {
    enter_scope(MAIN_SCOPE_STR);
  } else {
    int let_index = ++scope_counters.top();
    std::ostringstream ss;
    ss << let_index;
    enter_scope(ss.str());
  }
  for (auto type : node.types) {
    SymbolInfoPtr info(new SymbolInfo);
    info->category = SymbolCategory::TYPE;
    info->local_name = type->name->value;
    info->declared_info = *type;
    info->flattened_info = flatten_type(current_scope, *type);
    symtab->add(current_scope, info);
  }
  for (auto var : node.vars) {
    for (auto name : var->names) {
      SymbolInfoPtr info(new SymbolInfo);
      info->category = SymbolCategory::VARIABLE;
      info->local_name = name->value;
      info->declared_info = *var;
      info->flattened_info = flatten_type(current_scope, *var);
      symtab->add(current_scope, info);
    }
  }
  for (auto func : node.funcs) {
    SymbolInfoPtr info(new SymbolInfo);
    info->category = SymbolCategory::FUNCTION;
    info->local_name = func->name->value;
    if (func->has_return_type()) {
      info->declared_info = *func->return_type;
      info->flattened_info = flatten_type(current_scope, *func->return_type);
    } else {
      info->has_type = false;
    }
    for (auto param : func->params) {
      info->param_names.push_back(param->names[0]->value);
    }
    symtab->add(current_scope, info);
    func->accept(*this);
  }
  node.StmtSeqNode::accept(*this);
  exit_scope();
}

void SymtabBuilder::visit(FuncDeclNode& node) {
  enter_scope(node.name->value);
  for (int index = 0; index < node.params.size(); ++index) {
    auto param = node.params[index];
    SymbolInfoPtr info(new SymbolInfo);
    info->category = SymbolCategory::ARGUMENT;
    info->local_name = param->names[0]->value;
    info->declared_info = *param;
    info->flattened_info = flatten_type(current_scope, *param);
    info->argument_position = index;
    symtab->add(current_scope, info);
  }
  node.StmtSeqNode::accept(*this);
  exit_scope();
}

void ScopedSymbolTable::print(std::ostream& out) {
  for (auto name_info : symbols) {
    std::string name = name_info.first;
    SymbolInfoPtr info = name_info.second;
    out << scope.get_unique() << "\t" << info->unique_name << "\t"
        << static_cast<int>(info->category) << "\n";
  }
}

void SymbolTable::print(std::ostream& out) {
  for (auto scope_table : scoped_tables) {
    scope_table.second->print(out);
  }
}

SymbolInfoPtr ScopedSymbolTable::lookup(string name) const {
  ScopedSymbolTable::Iterator found = symbols.find(name);
  if (found == symbols.end()) {
    return SymbolInfo::NOT_FOUND;
  }
  return found->second;
}

vector<SymbolInfoPtr> ScopedSymbolTable::get_all_symbols() const {
  vector<SymbolInfoPtr> ret;
  for (auto symbol : symbols) {
    ret.push_back(symbol.second);
  }
  return ret;
}

SymbolInfoPtr SymbolTable::lookup(Scope scope, string name) const {
  do {
    SymbolTable::Iterator found_scoped_table;
    found_scoped_table = scoped_tables.find(scope);
    if (found_scoped_table != scoped_tables.end()) {
      auto scoped_table = found_scoped_table->second;
      SymbolInfoPtr found_symbol = scoped_table->lookup(name);
      if (found_symbol->local_name == name) {
        return found_symbol;
      }
    }
    scope = scope.exit();
  } while (!scope.is_empty());
  return shared_ptr<SymbolInfo>();
}

SymbolInfoPtr SymbolTable::scoped_lookup(Scope scope, string name) const {
  SymbolTable::Iterator found_scoped_table = scoped_tables.find(scope);
  if (found_scoped_table == scoped_tables.end()) {
    return SymbolInfo::NOT_FOUND;
  }
  auto scoped_table = found_scoped_table->second;
  return scoped_table->lookup(name);
}

vector<Scope> SymbolTable::get_all_scopes() const {
  vector<Scope> ret;
  for (auto key_value : scoped_tables) {
    ret.push_back(key_value.first);
  }
  return ret;
}

shared_ptr<ScopedSymbolTable> SymbolTable::get_scoped_table(Scope s) const {
  return scoped_tables.at(s);
}

FlattenedTypeInfo SymtabBuilder::flatten_type(Scope scope,
                                              const TypeInfoNode& node) const {
  FlattenedTypeInfo ret;
  ret.element_count = 1;
  const TypeInfoNode* local_node = &node;
  do {
    if (local_node->is_array()) {
      ret.element_count = ret.element_count * local_node->array_size->value;
      ret.is_array = true;
    }
    ret.base_type = local_node->base_type->value == "float"
                        ? NumericalType::FLOAT
                        : NumericalType::INT;
    string base_type = local_node->base_type->value;
    while (scope != Scope::GLOBAL) {
      SymbolInfoPtr info = symtab->scoped_lookup(scope, base_type);
      if (info && info->local_name == base_type &&
          info->category == SymbolCategory::TYPE) {
        local_node = &info->declared_info;
        break;
      }
      scope = scope.exit();
    }
  } while (scope != Scope::GLOBAL);
  return ret;
}
