/*
 * ast.cpp
 */

#include "SyntaxTree.hpp"

#include <memory>
#include <string>

void StringNode         ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void IntNode            ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void FloatNode          ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void ArrayAccessNode    ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void VariableAccessNode ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void FuncDeclNode       ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void VarDeclNode        ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void TypeDeclNode       ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void ProgramNode        ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void LetNode            ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void IfNode             ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void BinaryExprNode     ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void FuncCallNode       ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void AssignNode         ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void WhileNode          ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void ForNode            ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void BreakNode          ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void ReturnNode         ::accept(AstVisitor& visitor) { visitor.visit(*this); }
void StmtSeqNode        ::accept(AstVisitor& visitor) { visitor.visit(*this); }

void DumpTreeVisitor::visit(StringNode& node) {
  out << std::string(level, '\t') << "\"" << node.value << "\"\n";
}

void DumpTreeVisitor::visit(IntNode& node) {
  out << std::string(level, '\t') << "int(" << node.value << ")\n";
}

void DumpTreeVisitor::visit(FloatNode& node) {
  out << std::string(level, '\t') << "float(" << node.value << ")\n";
}

void DumpTreeVisitor::visit(ArrayAccessNode& node) {
  out << std::string(level, '\t') << node.name->value << "[\n";
  ++level;
  node.index->accept(*this);
  --level;
  out << std::string(level, '\t') << "]\n";
}
void DumpTreeVisitor::visit(TypeInfoNode& node) {
  if (node.is_array()) {
    out << "array [" << node.array_size->value << "] of ";
  }
  out << node.base_type->value;
}
void DumpTreeVisitor::visit(VariableAccessNode& node) {
  out << std::string(level, '\t') << node.name->value << "\n";
}
void DumpTreeVisitor::visit(FuncDeclNode& node) {
  out << std::string(level, '\t') << "function " << node.name->value << "(";
  if (!node.params.empty()) {
    int i = 0;
    for (std::shared_ptr<VarDeclNode> param : node.params) {
      if (i != 0) {
        out << ", ";
      }
      visit(*param, false);
      ++i;
    }
  }
  out << ")";
  if (node.has_return_type()) {
    out << ": ";
    visit(*node.return_type);
  }
  out << "\n";
  ++level;
  for (std::shared_ptr<StatementNode> stmt : node.stmts) {
    stmt->accept(*this);
  }
  --level;
}
void DumpTreeVisitor::visit(VarDeclNode& node) {
  visit(node, true);
}
void DumpTreeVisitor::visit(VarDeclNode& node, bool var_statement) {
  if (var_statement) {
    out << std::string(level, '\t') << "var ";
  }
  out << node.names[0]->value;
  for (int i = 1; i < node.names.size(); ++i) {
    out << ", " << node.names[i]->value;
  }
  out << ": ";
  visit(static_cast<TypeInfoNode&>(node));
  if (node.has_init_value()) {
    out << " := ";
    std::shared_ptr<IntNode> as_int =
        std::dynamic_pointer_cast<IntNode>(node.init_value);
    std::shared_ptr<FloatNode> as_float =
        std::dynamic_pointer_cast<FloatNode>(node.init_value);
    if (as_int) {
      out << as_int->value;
    } else {
      out << as_float->value;
    }
  }
  if (var_statement) {
    out << "\n";
  }
}
void DumpTreeVisitor::visit(TypeDeclNode& node) {
  out << std::string(level, '\t') << "type " << node.name->value << " = ";
  if (node.is_array()) {
    out << "array [" << node.array_size->value << "] of ";
  }
  out << node.base_type->value << "\n";
}
void DumpTreeVisitor::visit(ProgramNode& node) {
  out << std::string(level, '\t') << node.name << '\n';
  ++level;
  for (std::shared_ptr<TypeDeclNode> type : node.types) {
    type->accept(*this);
  }
  for (std::shared_ptr<VarDeclNode> type : node.vars) {
    type->accept(*this);
  }
  for (std::shared_ptr<FuncDeclNode> type : node.funcs) {
    type->accept(*this);
  }
  for (std::shared_ptr<StatementNode> stmt : node.stmts) {
    stmt->accept(*this);
  }
  --level;
}
void DumpTreeVisitor::visit(BinaryExprNode& node) {
  out << std::string(level, '\t') << node.operator_ << '\n';
  ++level;
  node.left->accept(*this);
  node.right->accept(*this);
  --level;
}
void DumpTreeVisitor::visit(FuncCallNode& node) {
  out << std::string(level, '\t') << node.name->value << "(\n";
  ++level;
  for (std::shared_ptr<ExprNode> expr : node.arguments) {
    expr->accept(*this);
  }
  --level;
  out << std::string(level, '\t') << ");\n";
}
void DumpTreeVisitor::visit(AssignNode& node) {
  out << std::string(level, '\t') << ":=\n";
  ++level;
  node.target->accept(*this);
  node.expression->accept(*this);
  --level;
}
void DumpTreeVisitor::visit(LetNode& node) {
  out << std::string(level, '\t') << "let\n";
  ++level;
  for (auto decl : node.types) {
    decl->accept(*this);
  }
  for (auto decl : node.vars) {
    decl->accept(*this);
  }
  for (auto decl : node.funcs) {
    decl->accept(*this);
  }
  out << std::string(level - 1, '\t') << "in\n";
  for (auto decl : node.stmts) {
    decl->accept(*this);
  }
  --level;
}
void DumpTreeVisitor::visit(IfNode& node) {
  out << std::string(level, '\t') << "if\n";
  ++level;
  node.condition->accept(*this);
  out << std::string(level - 1, '\t') << "then\n";
  node.body->accept(*this);
  if (node.has_else()) {
    out << std::string(level - 1, '\t') << "else\n";
    node.else_->accept(*this);
  }
  --level;
}
void DumpTreeVisitor::visit(WhileNode& node) {
  out << std::string(level, '\t') << "while\n";
  ++level;
  node.condition->accept(*this);
  out << std::string(level - 1, '\t') << "do\n";
  for (auto decl : node.stmts) {
    decl->accept(*this);
  }
  --level;
}
void DumpTreeVisitor::visit(ForNode& node) {
  out << std::string(level, '\t') << "for " << node.variable->value << "\n";
  ++level;
  out << std::string(level - 1, '\t') << "start\n";
  node.start->accept(*this);
  out << std::string(level - 1, '\t') << "stop\n";
  node.stop->accept(*this);
  out << std::string(level - 1, '\t') << "do\n";
  for (auto decl : node.stmts) {
    decl->accept(*this);
  }
  --level;
}
void DumpTreeVisitor::visit(BreakNode& node) {
  out << std::string(level, '\t') << "break\n";
}
void DumpTreeVisitor::visit(ReturnNode& node) {
  out << std::string(level, '\t') << "return\n";
  ++level;
  node.expression->accept(*this);
  --level;
}
