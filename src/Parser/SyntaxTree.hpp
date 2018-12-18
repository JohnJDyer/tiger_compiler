/*
 * AST representation.
 */

#ifndef SRC_PARSER_AST_H_
#define SRC_PARSER_AST_H_

#include <memory>
#include <ostream>
#include <string>
#include <vector>

class AstVisitor;

class AstNode {
 public:
  friend class AstVisitor;
  virtual ~AstNode() = default;
  int line_number;
  virtual void accept(AstVisitor& visitor) = 0;
};

class StatementNode : public AstNode {
 public:
  virtual ~StatementNode() = default;
};

class ExprNode : public StatementNode {
 public:
  virtual ~ExprNode() = default;
};

class LiteralNode : public ExprNode {
 public:
  virtual ~LiteralNode() = default;
};

class StringNode : public LiteralNode {
 public:
  virtual ~StringNode() = default;
  std::string value;
  virtual void accept(AstVisitor& visitor);
};

class NumericLiteralNode : public LiteralNode {
 public:
  virtual ~NumericLiteralNode() = default;
};

class IntNode : public NumericLiteralNode {
 public:
  virtual ~IntNode() = default;
  int value;
  virtual void accept(AstVisitor& visitor);
};

class FloatNode : public NumericLiteralNode {
 public:
  virtual ~FloatNode() = default;
  float value;
  virtual void accept(AstVisitor& visitor);
};

class BinaryExprNode : public ExprNode {
 public:
  friend class AstVisitor;
  virtual ~BinaryExprNode() = default;
  std::shared_ptr<ExprNode> left;
  std::shared_ptr<ExprNode> right;
  std::string operator_;
  virtual void accept(AstVisitor& visitor);
};

class VariableAccessNode : public ExprNode {
 public:
  virtual ~VariableAccessNode() = default;
  std::shared_ptr<StringNode> name;
  virtual void accept(AstVisitor& visitor);
};

class ArrayAccessNode : public VariableAccessNode {
 public:
  virtual ~ArrayAccessNode() = default;
  std::shared_ptr<ExprNode> index;
  virtual void accept(AstVisitor& visitor);
};

class TypeInfoNode : public AstNode {
 public:
  virtual ~TypeInfoNode() = default;
  std::shared_ptr<IntNode> array_size;
  bool is_array() const { return bool(array_size); }
  std::shared_ptr<StringNode> base_type;
  virtual void accept(AstVisitor& visitor) {}
};

class TypeDeclNode : public TypeInfoNode {
 public:
  virtual ~TypeDeclNode() = default;
  std::shared_ptr<StringNode> name;
  virtual void accept(AstVisitor& visitor);
};

class VarDeclNode : public TypeInfoNode {
 public:
  virtual ~VarDeclNode() = default;
  std::vector<std::shared_ptr<StringNode>> names;
  std::shared_ptr<NumericLiteralNode> init_value;
  bool has_init_value() const { return bool(init_value); }
  virtual void accept(AstVisitor& visitor);
};

class StmtSeqNode : public StatementNode {
 public:
  virtual ~StmtSeqNode() = default;
  std::vector<std::shared_ptr<StatementNode>> stmts;
  virtual void accept(AstVisitor& visitor);
};

class FuncDeclNode : public StmtSeqNode {
 public:
  virtual ~FuncDeclNode() = default;
  std::shared_ptr<StringNode> name;
  std::vector<std::shared_ptr<VarDeclNode>> params;
  std::shared_ptr<TypeInfoNode> return_type;
  bool has_return_type() const {
    return bool(return_type) && bool(return_type->base_type);
  }
  virtual void accept(AstVisitor& visitor);
};

class FuncCallNode : public ExprNode {
 public:
  virtual ~FuncCallNode() = default;
  std::shared_ptr<StringNode> name;
  std::vector<std::shared_ptr<ExprNode>> arguments;
  virtual void accept(AstVisitor& visitor);
};

class AssignNode : public StatementNode {
 public:
  virtual ~AssignNode() = default;
  std::shared_ptr<VariableAccessNode> target;
  std::shared_ptr<ExprNode> expression;
  virtual void accept(AstVisitor& visitor);
};

class LetNode : public StmtSeqNode {
 public:
  virtual ~LetNode() = default;
  std::vector<std::shared_ptr<TypeDeclNode>> types;
  std::vector<std::shared_ptr<VarDeclNode>> vars;
  std::vector<std::shared_ptr<FuncDeclNode>> funcs;
  virtual void accept(AstVisitor& visitor);
};

class ProgramNode : public LetNode {
 public:
  virtual ~ProgramNode() = default;
  std::string name;
  virtual void accept(AstVisitor& visitor);
};

class IfNode : public StatementNode {
 public:
  virtual ~IfNode() = default;
  std::shared_ptr<ExprNode> condition;
  std::shared_ptr<StmtSeqNode> body;
  std::shared_ptr<StmtSeqNode> else_;
  bool has_else() const { return bool(else_); }
  virtual void accept(AstVisitor& visitor);
};

class WhileNode : public StmtSeqNode {
 public:
  virtual ~WhileNode() = default;
  std::shared_ptr<ExprNode> condition;
  virtual void accept(AstVisitor& visitor);
};

class ForNode : public StmtSeqNode {
 public:
  virtual ~ForNode() = default;
  std::shared_ptr<StringNode> variable;
  std::shared_ptr<ExprNode> start;
  std::shared_ptr<ExprNode> stop;
  virtual void accept(AstVisitor& visitor);
};

class BreakNode : public StatementNode {
 public:
  virtual ~BreakNode() = default;
  virtual void accept(AstVisitor& visitor);
};

class ReturnNode : public StatementNode {
 public:
  virtual ~ReturnNode() = default;
  std::shared_ptr<ExprNode> expression;
  virtual void accept(AstVisitor& visitor);
};

#define DECLARE_VISIT_METHODS(EQ0)     \
  \
virtual void                           \
  visit(StringNode& node) EQ0;         \
  \
virtual void                           \
  visit(IntNode& node) EQ0;            \
  \
virtual void                           \
  visit(FloatNode& node) EQ0;          \
  \
virtual void                           \
  visit(ArrayAccessNode& node) EQ0;    \
  \
virtual void                           \
  visit(VariableAccessNode& node) EQ0; \
  \
virtual void                           \
  visit(FuncDeclNode& node) EQ0;       \
  \
virtual void                           \
  visit(VarDeclNode& node) EQ0;        \
  \
virtual void                           \
  visit(TypeDeclNode& node) EQ0;       \
  \
virtual void                           \
  visit(ProgramNode& node) EQ0;        \
  \
virtual void                           \
  visit(LetNode& node) EQ0;            \
  \
virtual void                           \
  visit(IfNode& node) EQ0;             \
  \
virtual void                           \
  visit(BinaryExprNode& node) EQ0;     \
  \
virtual void                           \
  visit(FuncCallNode& node) EQ0;       \
  \
virtual void                           \
  visit(AssignNode& node) EQ0;         \
  \
virtual void                           \
  visit(WhileNode& node) EQ0;          \
  \
virtual void                           \
  visit(ForNode& node) EQ0;            \
  \
virtual void                           \
  visit(BreakNode& node) EQ0;          \
  \
virtual void                           \
  visit(ReturnNode& node) EQ0;

class AstVisitor {
 public:
  virtual ~AstVisitor() = default;
  DECLARE_VISIT_METHODS({});
  virtual void visit(StmtSeqNode& node) {
    for (std::shared_ptr<StatementNode> stmt : node.stmts) {
      stmt->accept(*this);
    }
  }
};

class DumpTreeVisitor : public AstVisitor {
 public:
  DumpTreeVisitor(std::ostream& out, int level = 0) : out(out), level(level) {}
  DECLARE_VISIT_METHODS()

 private:
  void visit(VarDeclNode& node, bool var_statement);
  void visit(TypeInfoNode& node);

  std::ostream& out;
  int level;
};

#endif /* SRC_PARSER_AST_H_ */
