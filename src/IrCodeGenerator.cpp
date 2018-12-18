#include "IrCodeGenerator.hpp"

#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <set>
#include <sstream>
#include <vector>

#include "RegisterAllocator.hpp"

using namespace std;
using namespace IR;

void IrBuilder::visit(ProgramNode& node) {
  enter_scope(GLOBAL_SCOPE_STR);
  node.LetNode::accept(*this);
  insns.push_back(make_shared<ReturnInstruction>());
  exit_scope();
  assert(current_scope.is_empty());
  assert(label_stack.empty());
}

void IrBuilder::visit(LetNode& node) {
  string name;
  if (scope_counters.size() == 1) {
    name = MAIN_SCOPE_STR;
  } else {
    int let_index = ++scope_counters.top();
    ostringstream ss;
    ss << let_index;
    name = ss.str();
  }
  string label = generate_unique_name(current_scope, name);
  insns.push_back(make_shared<Label>(label));
  enter_scope(name);
  insns.push_back(make_shared<CreateProlog>());
  for (auto var : node.vars) {
    var->accept(*this);
  }
  label = new_label("let");
  insns.push_back(make_shared<GotoInstruction>(label));
  for (auto func : node.funcs) {
    func->accept(*this);
  }
  insns.push_back(make_shared<Label>(label));
  node.StmtSeqNode::accept(*this);
  exit_scope();
  assert(expr_stack.empty());
}

void IrBuilder::visit(ArrayAccessNode& node) {
  node.index->accept(*this);
  SymbolInfoPtr index = expr_stack.top();
  expr_stack.pop();
  SymbolInfoPtr array = symtab.lookup(current_scope, node.name->value);
  SymbolInfoPtr temp  = new_temp(array->flattened_info.base_type);
  insns.push_back(make_shared<ArrayOpInstruction>(
      ArrayOpInstruction::read(array, index, temp)));
  expr_stack.push(temp);
}

void IrBuilder::visit(AssignNode& node) {
  // TODO prevent int := float
  shared_ptr<FuncCallNode> func_call =
      dynamic_pointer_cast<FuncCallNode>(node.expression);
  if (func_call) {
    visit(*func_call, true /* store_result */);
  } else {
    node.expression->accept(*this);
  }
  SymbolInfoPtr source = expr_stack.top();
  expr_stack.pop();

  shared_ptr<ArrayAccessNode> array_access =
      dynamic_pointer_cast<ArrayAccessNode>(node.target);
  if (array_access) {
    visit(*array_access->index, NumericalType::INT);  // index is an int
    SymbolInfoPtr index = expr_stack.top();
    expr_stack.pop();
    SymbolInfoPtr array =
        symtab.lookup(current_scope, array_access->name->value);
    if (source->flattened_info.base_type != array->flattened_info.base_type) {
      SymbolInfoPtr temp = new_temp(array->flattened_info.base_type);
      insns.push_back(make_shared<Comment>(
          (source->flattened_info.base_type == NumericalType::INT
               ? string("int")
               : string("float")) +
          " -> " +
          (array->flattened_info.base_type == NumericalType::INT ? "int"
                                                                 : "float")));
      insns.push_back(make_shared<AssignInstruction>(temp, source));
      source = temp;
    }
    insns.push_back(make_shared<ArrayOpInstruction>(
        ArrayOpInstruction::write(array, index, source)));
  } else {
    node.target->accept(*this);
    SymbolInfoPtr dest = expr_stack.top();
    expr_stack.pop();
    if (source->flattened_info.base_type != dest->flattened_info.base_type) {
      insns.push_back(make_shared<Comment>(
          (source->flattened_info.base_type == NumericalType::INT
               ? string("int")
               : string("float")) +
          " -> " +
          (dest->flattened_info.base_type == NumericalType::INT ? "int"
                                                                : "float")));
    }
    insns.push_back(make_shared<AssignInstruction>(dest, source));
  }
}

#define IF_TYPE(type, n) if (const type* typed = dynamic_cast<const type*>(n))

static bool is_comparison(const ExprNode* node) {
  IF_TYPE(BinaryExprNode, node) {
    return (typed->operator_ == "=" || typed->operator_ == "<>" ||
            typed->operator_ == "<" || typed->operator_ == "<=" ||
            typed->operator_ == ">" || typed->operator_ == ">=");
  }
  return false;
}

NumericalType IrBuilder::get_type(const ExprNode* node) const {
  NumericalType ret = NumericalType::INT;
  queue<const ExprNode*> work_set;
  work_set.push(node);
  while (!work_set.empty()) {
    const ExprNode* current = work_set.front();
    work_set.pop();
    IF_TYPE(FloatNode, current) {
      ret = NumericalType::FLOAT;
      break;
    }
    IF_TYPE(BinaryExprNode, current) {
      if (is_comparison(typed)) {  // Comparison returns an int
        continue;
      }
      work_set.push(typed->left.get());
      work_set.push(typed->right.get());
      continue;
    }
    IF_TYPE(VariableAccessNode, current) {
      SymbolInfoPtr info = symtab.lookup(current_scope, typed->name->value);
      if (info->flattened_info.base_type == NumericalType::FLOAT) {
        ret = NumericalType::FLOAT;
        break;
      }
    }
    IF_TYPE(FuncCallNode, current) {
      SymbolInfoPtr info = symtab.lookup(current_scope, typed->name->value);
      if (info->has_type &&
          info->flattened_info.base_type == NumericalType::FLOAT) {
        ret = NumericalType::FLOAT;
        break;
      }
    }
  }
  return ret;
}

void IrBuilder::coerce(NumericalType coerced_type) {
  // TODO array
  SymbolInfoPtr top = expr_stack.top();
  if (top->flattened_info.base_type != coerced_type) {
    expr_stack.pop();
    SymbolInfoPtr temp = new_temp(coerced_type);
    insns.push_back(make_shared<Comment>(
        (top->flattened_info.base_type == NumericalType::INT
             ? string("int")
             : string("float")) +
        " -> " + (coerced_type == NumericalType::INT ? "int" : "float")));
    insns.push_back(make_shared<AssignInstruction>(temp, top));
    expr_stack.push(temp);
  }
}

void IrBuilder::visit(ExprNode& node, NumericalType coerced_type) {
  IF_TYPE(BinaryExprNode, &node) {
    visit(const_cast<BinaryExprNode&>(*typed), coerced_type);
  }
  else {
    node.accept(*this);
  }
  coerce(coerced_type);
}

void IrBuilder::visit(BinaryExprNode& node) {
  NumericalType coerced = get_type(&node);
  visit(node, coerced);
}

void IrBuilder::visit(BinaryExprNode& node, NumericalType coerced_type) {
  SymbolInfoPtr left;
  SymbolInfoPtr right;
  SymbolInfoPtr tmp;
  string op;

  if (is_comparison(&node)) {
    // comparison always returns an int
    // but the comparands must be of the same type
    // so we evaluate each comparand independently
    // and promote the resulting type if needed
    NumericalType left_type = get_type(node.left.get());
    NumericalType right_type = get_type(node.right.get());
    visit(*node.left, left_type);
    if (left_type != right_type) {
      coerce(NumericalType::FLOAT);
    }
    left = expr_stack.top();
    expr_stack.pop();
    visit(*node.right, right_type);
    if (left_type != right_type) {
      coerce(NumericalType::FLOAT);
    }
    right = expr_stack.top();
    expr_stack.pop();
    tmp = new_temp(NumericalType::INT);  // always INT
    insns.push_back(make_shared<AssignInstruction>(tmp, new_literal(1)));
    string label = new_label("compare", false /* store_label */);
    string branch;
    if (node.operator_ == "=") {
      branch = "breq";
    } else if (node.operator_ == "<>") {
      branch = "brneq";
    } else if (node.operator_ == "<") {
      branch = "brlt";
    } else if (node.operator_ == "<=") {
      branch = "brleq";
    } else if (node.operator_ == ">") {
      branch = "brgt";
    } else if (node.operator_ == ">=") {
      branch = "brgeq";
    } else {
      throw std::out_of_range("Unknown Binary Operator: " + node.operator_);
    }
    insns.push_back(make_shared<BranchInstruction>(branch, left, right, label));
    insns.push_back(make_shared<AssignInstruction>(tmp, new_literal(0)));
    insns.push_back(make_shared<Label>(label));
    expr_stack.push(tmp);
    coerce(coerced_type);
  } else {
    // arithmetic operation
    // we need to coerce from the deepest node, so pass it down from here
    visit(*node.left, coerced_type);
    left = expr_stack.top();
    expr_stack.pop();
    visit(*node.right, coerced_type);
    right = expr_stack.top();
    expr_stack.pop();
    switch (node.operator_[0]) {
      case '|':
        op = "or";
        break;
      case '&':
        op = "and";
        break;
      case '+':
        op = "add";
        break;
      case '-':
        op = "sub";
        break;
      case '*':
        op = "mult";
        break;
      case '/':
        op = "div";
        break;
      default:
        throw std::out_of_range("Unknown arithmetic operator: " +
                                node.operator_);
    }
    tmp = new_temp(coerced_type);
    insns.push_back(make_shared<BinOpInstruction>(op, tmp, left, right));
    expr_stack.push(tmp);
  }
}

void IrBuilder::visit(BreakNode& node) {
  insns.push_back(make_shared<GotoInstruction>(label_stack.top()));
}

void IrBuilder::visit(FuncCallNode& node) {
  visit(node, false /* store_result */);
}

void IrBuilder::visit(FuncCallNode& node, bool store_result) {
  SymbolInfoPtr function = symtab.lookup(current_scope, node.name->value);
  Scope function_scope = function->scope.enter(node.name->value);
  vector<SymbolInfoPtr> args;
  for (int i = 0; i < node.arguments.size(); ++i) {
    // TODO array parameters
    auto expr = node.arguments[i];
    expr->accept(*this);
    SymbolInfoPtr arg_info =
        symtab.lookup(function_scope, function->param_names[i]);
    coerce(arg_info->flattened_info.base_type);
    args.push_back(expr_stack.top());
    expr_stack.pop();
  }
  if (store_result) {
    // TODO array return value
    SymbolInfoPtr dest = new_temp(function->flattened_info.base_type);
    insns.push_back(make_shared<CallInstruction>(dest, function, args));
    expr_stack.push(dest);
  } else {
    insns.push_back(make_shared<CallInstruction>(function, args));
  }
}

void IrBuilder::visit(FuncDeclNode& node) {
  string label = generate_unique_name(current_scope, node.name->value);
  insns.push_back(make_shared<Label>(label));
  enter_scope(node.name->value);
  ostringstream ss;
  ss << node.params.size() << " params";
  for (int i = 0; i < node.params.size(); ++i) {
    auto param = node.params[i];
    ss << " " << param->names[0]->value;
  }
  insns.push_back(make_shared<Comment>(ss.str()));
  insns.push_back(make_shared<CreateProlog>());
  node.StmtSeqNode::accept(*this);
  if (!node.has_return_type()) {
    insns.push_back(make_shared<ReturnInstruction>());
  }
  exit_scope();
}

void IrBuilder::visit(IfNode& node) {
  node.condition->accept(*this);
  SymbolInfoPtr cond_result = expr_stack.top();
  expr_stack.pop();
  string else_label = new_label("if", false /* store_label*/);
  string end_label;
  insns.push_back(make_shared<BranchInstruction>("breq", cond_result,
                                                 new_literal(0), else_label));
  node.body->accept(*this);
  if (node.else_) {
    end_label = new_label("if", false /* store_label*/);
    insns.push_back(make_shared<GotoInstruction>(end_label));
  }
  insns.push_back(make_shared<Label>(else_label));
  if (node.else_) {
    node.else_->accept(*this);
    insns.push_back(make_shared<Label>(end_label));
  }
}

void IrBuilder::visit(VarDeclNode& node) {
  if (!node.has_init_value()) {
    return;  // no initial value, no need codegen required
  }
  SymbolInfoPtr base_type = symtab.lookup(current_scope, node.base_type->value);
  FlattenedTypeInfo type_info = base_type->flattened_info;
  SymbolInfoPtr init_value;
  auto maybe_int_value = dynamic_pointer_cast<IntNode>(node.init_value);
  if (maybe_int_value) {
    init_value = new_literal(maybe_int_value->value);
  } else {
    init_value =
        new_literal(dynamic_pointer_cast<FloatNode>(node.init_value)->value);
  }
  for (auto name : node.names) {
    shared_ptr<AssignInstruction> insn;
    SymbolInfoPtr var = symtab.lookup(current_scope, name->value);
    if (type_info.is_array) {
      insn = make_shared<AssignInstruction>(var, type_info.element_count,
                                            init_value);
    } else {
      insn = make_shared<AssignInstruction>(var, init_value);
    }
    insn->numerical_type = type_info.base_type;
    insns.push_back(insn);
  }
}

void IrBuilder::visit(VariableAccessNode& node) {
  expr_stack.push(symtab.lookup(current_scope, node.name->value));
}

void IrBuilder::visit(WhileNode& node) {
  string begin_label = new_label("while", false /* store_label */);
  string end_label = new_label("while", true /* store_label */);
  insns.push_back(make_shared<Label>(begin_label));
  node.condition->accept(*this);
  insns.push_back(make_shared<BranchInstruction>("breq", expr_stack.top(),
                                                 new_literal(0), end_label));
  expr_stack.pop();
  node.StmtSeqNode::accept(*this);
  insns.push_back(make_shared<GotoInstruction>(begin_label));
  insns.push_back(make_shared<Label>(end_label));
  label_stack.pop();
}

void IrBuilder::visit(ForNode& node) {
  node.start->accept(*this);
  SymbolInfoPtr start_result = expr_stack.top();
  expr_stack.pop();
  SymbolInfoPtr id = symtab.lookup(current_scope, node.variable->value);
  insns.push_back(make_shared<AssignInstruction>(id, start_result));
  string test = new_label("for", false /* store_label */);
  insns.push_back(make_shared<Label>(test));
  node.stop->accept(*this);
  SymbolInfoPtr stop_result = expr_stack.top();
  expr_stack.pop();
  string end = new_label("for", true /* store_label */);
  insns.push_back(make_shared<BranchInstruction>("brgt", id, stop_result, end));
  node.StmtSeqNode::accept(*this);
  insns.push_back(make_shared<BinOpInstruction>("add", id, id, new_literal(1)));
  insns.push_back(make_shared<GotoInstruction>(test));
  insns.push_back(make_shared<Label>(end));
  label_stack.pop();
}

void IrBuilder::visit(FloatNode& node) {
  expr_stack.push(new_literal(node.value));
}

void IrBuilder::visit(IntNode& node) {
  expr_stack.push(new_literal(node.value));
}

void IrBuilder::visit(ReturnNode& node) {
  // TODO array return
  node.expression->accept(*this);
  SymbolInfoPtr function_info =
      symtab.lookup(current_scope.exit(), current_scope.get_local());
  coerce(function_info->flattened_info.base_type);
  insns.push_back(make_shared<ReturnInstruction>(expr_stack.top()));
  expr_stack.pop();
}

string IrBuilder::new_label(string key, bool store_label) {
  unordered_map<string, int>::iterator found = label_counters.find(key);
  if (found == label_counters.end()) {
    label_counters[key] = 0;
  }
  ostringstream ss;
  ss << key << "_" << ++label_counters[key];
  if (store_label) {
    label_stack.push(ss.str());
  }
  return ss.str();
}

SymbolInfoPtr IrBuilder::new_temp(NumericalType type) {
  int index = ++scope_counters.top();
  ostringstream name;
  name << "_tmp_" << index;
  SymbolInfoPtr info(new SymbolInfo);
  info->local_name = name.str();
  info->category = SymbolCategory::TEMPORARY;
  info->flattened_info.base_type = type;
  symtab.add(current_scope, info);
  return info;
}

SymbolInfoPtr IrBuilder::new_literal(int value) {
  SymbolInfoPtr info(new SymbolInfo);
  info->category = SymbolCategory::LITERAL;
  info->int_value = value;
  info->has_type = true;
  ostringstream name;
  name << '#' << value;
  info->local_name = info->unique_name = name.str();
  auto type_string = make_shared<StringNode>();
  type_string->value = "int";
  info->declared_info.base_type = type_string;
  info->flattened_info.base_type = NumericalType::INT;
  info->flattened_info.element_count = 1;
  return info;
}

SymbolInfoPtr IrBuilder::new_literal(float value) {
  SymbolInfoPtr info(new SymbolInfo);
  info->category = SymbolCategory::LITERAL;
  info->float_value = value;
  info->has_type = true;
  ostringstream name;
  name << '#' << value;
  if (name.str().find('.') == string::npos) {
    name << ".0";
  }
  info->local_name = info->unique_name = name.str();
  auto type_string = make_shared<StringNode>();
  type_string->value = "float";
  info->declared_info.base_type = type_string;
  info->flattened_info.base_type = NumericalType::FLOAT;
  info->flattened_info.element_count = 1;
  return info;
}

void IrBuilder::visit(StringNode& node) {}
void IrBuilder::visit(TypeDeclNode& node) {}
