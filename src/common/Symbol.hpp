/**
 * Terminal
 *
 * @author:  Gatech Buzz
 */
#pragma once
#include<cstdio>
#include<iostream>
#include<fstream>
#include<ctype.h>

#include<algorithm>
#include<string>
#include<set>
#include <stack>
#include <map>
#include <utility>

enum Entry {
  Variables,
  Constants,
  Types,
  Functions,
  Temporaries
};

#define APPEND_COMMA(x) x,
#define QUOTE_THEN_APPEND_COMMA(x) #x,
#define MAKE_ENUM(enum_name, enum_def, initial_value) \
  enum class enum_name { \
    __FIRST = initial_value - 1, \
    enum_def(APPEND_COMMA) \
    __LAST \
  }; \
  static std::string convert##enum_name##ToString(enum_name value) { \
    static std::string names[] = { \
        enum_def(QUOTE_THEN_APPEND_COMMA) \
    }; \
    if (value > enum_name::__FIRST && value < enum_name::__LAST) { \
      int index = static_cast<int>(value) - static_cast<int>(enum_name::__FIRST) - 1; \
      return std::string(#enum_name) + "::" + names[index]; \
    } \
    return "Invalid"; \
  } \
  static bool isIn##enum_name##Range(int value) { \
    return (value > static_cast<int>(enum_name::__FIRST) && value < static_cast<int>(enum_name::__LAST)); \
  }

#define TerminalEnumDef(def) \
  /* Keyword tokens.
   * DO NOT change the order of keywords. Scanner.hpp expects this order. */ \
  def(ARRAY) \
  def(BREAK) \
  def(DO) \
  def(ELSE) \
  def(END) \
  def(FOR) \
  def(FUNCTION) \
  def(IF) \
  def(IN) \
  def(LET) \
  def(OF) \
  def(THEN) \
  def(TO) \
  def(TYPE) \
  def(VAR) \
  def(WHILE) \
  def(ENDIF) \
  def(BEGIN) \
  def(ENDDO) \
  def(RETURN) \
  /* Types */ \
  def(INT) \
  def(FLOAT) \
  /* Regular tokens */ \
  def(COMMA) \
  def(COLON) \
  def(SEMI) \
  \
  \
  def(LBRACK) \
  def(RBRACK) \
  def(LBRACE) \
  def(RBRACE) \
  def(PERIOD) \
  def(MULT) \
  def(DIV) \
  def(PLUS) \
  def(MINUS) \
  def(EQ) \
  def(NEQ) \
  def(LESSER) \
  def(GREATER) \
  def(LESSEREQ) \
  def(GREATEREQ) \
  def(AND) \
  def(OR) \
  def(ASSIGN) \
  def(LPAREN) \
  def(RPAREN) \
/* Type tokens */ \
  def(ID) \
  def(INTLIT) \
  def(FLOATLIT) \
  /* NULL token */ \
  def(NULLL) \
  /* End of file token */ \
  def(EOFF) \

#define NonterminalEnumDef(def) \
  def(TIGER_PROGRAM) \
  def(DECLARATION_SEGMENT) \
  def(TYPE_DECLARATION_LIST) \
  def(VAR_DECLARATION_LIST) \
  def(FUNCT_DECLARATION_LIST) \
  def(TYPE_DECLARATION) \
  def(TYPE) \
  def(TYPE_ID) \
  def(VAR_DECLARATION) \
  def(ID_LIST) \
  def(ID_LIST_TAIL) \
  def(OPTIONAL_INIT) \
  def(FUNCT_DECLARATION) \
  def(PARAM_LIST) \
  def(PARAM_LIST_TAIL) \
  def(RET_TYPE) \
  def(PARAM) \
  def(STAT_SEQ) \
  def(STAT_SEQ_TAIL) \
  def(STAT) \
  def(ASSIGN_OR_CALL) \
  def(ASSIGN_OR_CALL_TAIL) \
  def(EXPR_OR_CALL) \
  def(EXPR_OR_CALL_TAIL) \
  def(EXPR_AFTER_FACTOR) \
  def(IF_TAIL) \
  def(EXPR) \
  def(EXPR_TAIL) \
  def(LOGICAL_OPERATOR) \
  def(LOGICAL_OPERAND) \
  def(LOGICAL_OPERAND_TAIL) \
  def(COMPARATOR) \
  def(COMPARAND) \
  def(COMPARAND_TAIL) \
  def(PLUS_MINUS) \
  def(TERM) \
  def(TERM_TAIL) \
  def(MUL_DIV) \
  def(FACTOR) \
  def(CONST) \
  def(EXPR_LIST) \
  def(EXPR_LIST_TAIL) \
  def(LVALUE) \
  def(LVALUE_TAIL)

#define ActionEnumDef(def) \
  def(StatementRecoveryCheckpoint) \
  def(LetRecoveryCheckpoint) \
  def(PushString) \
  def(PushInt) \
  def(PushFloat) \
  def(CreateModuleNode) \
  def(CreateTypeDeclNode) \
  def(SetTypeArraySize) \
  def(SetTypeBaseType) \
  def(AddTypeToList) \
  def(CreateVarDeclNode) \
  def(AddIdToVar) \
  def(SetVarInitValue) \
  def(AddVarToList) \
  def(CreateFuncDeclNode) \
  def(AddParamToFunc) \
  def(CreateTypeInfoNode) \
  def(SetFuncReturnType) \
  def(AddFuncToList) \
  def(CreateBinaryExpr) \
  def(CreateArrayAccess) \
  def(CreateVariableAccess) \
  def(CreateFuncCallNode) \
  def(AddExprToArguments) \
  def(AddStatement) \
  def(CreateAssignNode) \
  def(CreateStmtSeqNode) \
  def(CreateIfNode) \
  def(AddStmtSeqToIfBody) \
  def(AddStmtSeqToElse) \
  def(CreateWhileNode) \
  def(CreateForNode) \
  def(SetForStart) \
  def(SetForStop) \
  def(CreateBreakNode) \
  def(CreateReturnNode) \
  def(CreateLetNode) \


class Symbol {
 public:
  // IMPORTANT: Do not change the initial value of 0 for Terminal!
  // Scanner.hpp assumes keywords start from 0.
  MAKE_ENUM(Terminal, TerminalEnumDef, 0)

  MAKE_ENUM(Nonterminal, NonterminalEnumDef, 20000)

  MAKE_ENUM(Action, ActionEnumDef, 40000)

  Symbol(Terminal v)
      : value(static_cast<int>(v)) {
  }

  Symbol(Nonterminal v)
      : value(static_cast<int>(v)) {
  }

  Symbol(Action v)
      : value(static_cast<int>(v)) {
  }

  Symbol(int v)
      : value(v) {
  }

  Symbol()
      : Symbol(0) {
  }

  std::string toString() const {
    if (isTerminal()) {
      return convertTerminalToString(static_cast<Terminal>(value));
    } else if (isNonterminal()) {
      return convertNonterminalToString(static_cast<Nonterminal>(value));
    } else if (isAction()) {
      return convertActionToString(static_cast<Action>(value));
    }
    return "Invalid";
  }

  std::string toShortString() const {
    std::string s(toString());
    return toString().substr(s.find_last_of(':') + 1);
  }

  bool isTerminal() const {
    return isInTerminalRange(value);
  }

  bool isNonterminal() const {
    return isInNonterminalRange(value);
  }

  bool isAction() const {
    return isInActionRange(value);
  }

  bool operator==(const Symbol& other) const {
    return value == other.value;
  }

  bool operator!=(const Symbol& other) const {
    return !operator==(other);
  }

  bool operator<(const Symbol& other) const {
    return value < other.value;
  }

  size_t hash() const {
    return value;
  }

  //jdyer
  Action getAction() const {
    return static_cast<Action>(value);
  }

  Terminal getTerminal() const {
    return static_cast<Terminal>(value);
  }

  private:
    int value;
};

using SymbolTerminalPair = std::pair<Symbol, Symbol>;

namespace std {
  template <>
  struct hash<Symbol>
  {
    std::size_t operator()(const Symbol& symbol) const {
      return symbol.hash();
    }
  };

  template <>
  struct hash<SymbolTerminalPair>
  {
    std::size_t operator()(const SymbolTerminalPair& pair) const {
      return pair.first.hash() * 1337 + pair.second.hash();
    }
  };
}
