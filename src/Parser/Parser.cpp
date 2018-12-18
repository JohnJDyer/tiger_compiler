/**
 * Parser
 *
 * @author: Gatech Buzz
 */
#include <cassert>
#include <cstring>
#include <string>
#include <unordered_map>
#include "../Scanner/Scanner.hpp"
#include "../SemanticAnalyzer/SymbolTable.hpp"
#include "SyntaxTree.hpp"
#include "../IrCodeGenerator.hpp"
#include "../RegisterAllocator.hpp"
#include "../MipsCodeGenerator.hpp"
#include "../CFGGenerator.hpp"

class ParseTable {
 public:
  ParseTable& from(Symbol nonterminal) {
    assert(nonterminal.isNonterminal());
    current_rule = nonterminal;
    return *this;
  }

  template <typename... T>
  ParseTable& each(T... symbols) {
    return each({symbols...});
  }

  template <typename... T>
  ParseTable& produce(T... symbols) {
    return produce({symbols...});
  }

  bool find(const SymbolTerminalPair& pair, std::vector<Symbol>& out) const {
    auto found = parseTable.find(pair);
    if (found == parseTable.end()) {
      return false;
    }
    out = found->second;
    return true;
  }

  bool find(Symbol nonterminal, Symbol terminal,
            std::vector<Symbol>& out) const {
    return find(SymbolTerminalPair(nonterminal, terminal), out);
  }

  // Finds all terminals that can expand start.
  std::vector<Symbol> findTerminals(Symbol start) {
    if (start.isTerminal()) {
      return {start};
    }
    std::vector<Symbol> ret;
    for (auto iter = parseTable.begin(); iter != parseTable.end(); iter++) {
      SymbolTerminalPair pair = iter->first;
      if (pair.first == start) {
        ret.push_back(pair.second);
      }
    }
    return ret;
  }

 private:
  ParseTable& each(std::initializer_list<Symbol> symbols) {
    possible_next_tokens = symbols;
    return *this;
  }

  ParseTable& produce(std::initializer_list<Symbol> symbols) {
    for (Symbol s : possible_next_tokens) {
      parseTable[SymbolTerminalPair(current_rule, s)] = symbols;
    }
    return *this;
  }

  Symbol current_rule;
  std::vector<Symbol> possible_next_tokens;
  std::unordered_map<SymbolTerminalPair, std::vector<Symbol>> parseTable;
};

class Parser {
 private:
  Scanner scanner;
  std::string globalFileName;
  std::stack<Symbol> parseStack;
  std::stack<std::shared_ptr<AstNode>> astStack;
  TokenPair currentToken;
  int currentLine;
  ParseTable parseTable;

 public:
  int numErrors;
  std::ofstream outFile;
  bool printDebug;

  Parser(std::string fileName)
      : scanner(fileName), numErrors(0), currentLine(0), printDebug(false) {

    globalFileName = fileName;

    parseTable.from(Symbol::Nonterminal::TIGER_PROGRAM)
        .each(Symbol::Terminal::LET)
        .produce(Symbol::Terminal::LET,
                 Symbol::Action::CreateModuleNode,
                 Symbol::Nonterminal::DECLARATION_SEGMENT,
                 Symbol::Terminal::IN,
                 Symbol::Nonterminal::STAT_SEQ,
                 Symbol::Terminal::END);

    parseTable.from(Symbol::Nonterminal::DECLARATION_SEGMENT)
        .each(Symbol::Terminal::TYPE, 
              Symbol::Terminal::VAR,
              Symbol::Terminal::FUNCTION, 
              Symbol::Terminal::IN)
        .produce(Symbol::Nonterminal::TYPE_DECLARATION_LIST,
                 Symbol::Nonterminal::VAR_DECLARATION_LIST,
                 Symbol::Nonterminal::FUNCT_DECLARATION_LIST);

    parseTable.from(Symbol::Nonterminal::TYPE_DECLARATION_LIST)
        .each(Symbol::Terminal::TYPE)
        .produce(Symbol::Nonterminal::TYPE_DECLARATION,
                 Symbol::Action::StatementRecoveryCheckpoint,
                 Symbol::Action::AddTypeToList,
                 Symbol::Nonterminal::TYPE_DECLARATION_LIST)

        .each(Symbol::Terminal::VAR, 
              Symbol::Terminal::FUNCTION,
              Symbol::Terminal::IN)
        .produce();

    parseTable.from(Symbol::Nonterminal::VAR_DECLARATION_LIST)
        .each(Symbol::Terminal::VAR)
        .produce(Symbol::Nonterminal::VAR_DECLARATION,
                 Symbol::Action::StatementRecoveryCheckpoint,
                 Symbol::Action::AddVarToList,
                 Symbol::Nonterminal::VAR_DECLARATION_LIST)
        
        .each(Symbol::Terminal::FUNCTION, 
              Symbol::Terminal::IN)
        .produce();

    parseTable.from(Symbol::Nonterminal::FUNCT_DECLARATION_LIST)
        .each(Symbol::Terminal::FUNCTION)
        .produce(Symbol::Nonterminal::FUNCT_DECLARATION,
                 Symbol::Action::StatementRecoveryCheckpoint,
                 Symbol::Nonterminal::FUNCT_DECLARATION_LIST)
        
        .each(Symbol::Terminal::IN)
        .produce();

    parseTable.from(Symbol::Nonterminal::TYPE_DECLARATION)
        .each(Symbol::Terminal::TYPE)
        .produce(Symbol::Terminal::TYPE,
                 Symbol::Action::PushString,
                 Symbol::Action::CreateTypeDeclNode,
                 Symbol::Terminal::ID,
                 Symbol::Terminal::EQ, 
                 Symbol::Nonterminal::TYPE,
                 Symbol::Terminal::SEMI);

    parseTable.from(Symbol::Nonterminal::TYPE)
        .each(Symbol::Terminal::ARRAY)
        .produce(Symbol::Terminal::ARRAY, 
                 Symbol::Terminal::LBRACK,
                 Symbol::Action::PushInt,
                 Symbol::Action::SetTypeArraySize,
                 Symbol::Terminal::INTLIT, 
                 Symbol::Terminal::RBRACK,
                 Symbol::Terminal::OF,
                 Symbol::Action::PushString,
                 Symbol::Action::SetTypeBaseType,
                 Symbol::Nonterminal::TYPE_ID)
        
        .each(Symbol::Terminal::ID)
        .produce(Symbol::Action::PushString,
                 Symbol::Action::SetTypeBaseType,
                 Symbol::Terminal::ID)
        
        .each(Symbol::Terminal::INT,
              Symbol::Terminal::FLOAT)
        .produce(Symbol::Action::PushString,
                 Symbol::Action::SetTypeBaseType,
                 Symbol::Nonterminal::TYPE_ID);

    parseTable.from(Symbol::Nonterminal::TYPE_ID)
        .each(Symbol::Terminal::INT)
        .produce(Symbol::Terminal::INT)
        
        .each(Symbol::Terminal::FLOAT)
        .produce(Symbol::Terminal::FLOAT);

    parseTable.from(Symbol::Nonterminal::VAR_DECLARATION)
        .each(Symbol::Terminal::VAR)
        .produce(Symbol::Terminal::VAR,
                 Symbol::Action::CreateVarDeclNode,
                 Symbol::Nonterminal::ID_LIST,
                 Symbol::Terminal::COLON, 
                 Symbol::Nonterminal::TYPE,
                 Symbol::Nonterminal::OPTIONAL_INIT,
                 Symbol::Terminal::SEMI);

    parseTable.from(Symbol::Nonterminal::ID_LIST)
        .each(Symbol::Terminal::ID)
        .produce(Symbol::Action::PushString,
                 Symbol::Action::AddIdToVar,
                 Symbol::Terminal::ID,
                 Symbol::Nonterminal::ID_LIST_TAIL);

    parseTable.from(Symbol::Nonterminal::ID_LIST_TAIL)
        .each(Symbol::Terminal::COMMA)
        .produce(Symbol::Terminal::COMMA, 
                 Symbol::Nonterminal::ID_LIST)
        
        .each(Symbol::Terminal::COLON)
        .produce();

    parseTable.from(Symbol::Nonterminal::OPTIONAL_INIT)
        .each(Symbol::Terminal::ASSIGN)
        .produce(Symbol::Terminal::ASSIGN,
                 Symbol::Nonterminal::CONST,
                 Symbol::Action::SetVarInitValue)
        
        .each(Symbol::Terminal::SEMI)
        .produce();

    parseTable.from(Symbol::Nonterminal::FUNCT_DECLARATION)
        .each(Symbol::Terminal::FUNCTION)
        .produce(Symbol::Terminal::FUNCTION,
                 Symbol::Action::PushString,
                 Symbol::Action::CreateFuncDeclNode,
                 Symbol::Terminal::ID,
                 Symbol::Terminal::LPAREN,
                 Symbol::Nonterminal::PARAM_LIST,
                 Symbol::Terminal::RPAREN,
                 Symbol::Action::CreateTypeInfoNode,
                 Symbol::Nonterminal::RET_TYPE,
                 Symbol::Action::SetFuncReturnType,
                 Symbol::Terminal::BEGIN, 
                 Symbol::Nonterminal::STAT_SEQ,
                 Symbol::Terminal::END,
                 Symbol::Action::AddFuncToList,
                 Symbol::Terminal::SEMI);

    parseTable.from(Symbol::Nonterminal::PARAM_LIST)
        .each(Symbol::Terminal::ID)
        .produce(Symbol::Nonterminal::PARAM,
                 Symbol::Nonterminal::PARAM_LIST_TAIL)
                 
        .each(Symbol::Terminal::RPAREN)
        .produce();

    parseTable.from(Symbol::Nonterminal::PARAM_LIST_TAIL)
        .each(Symbol::Terminal::COMMA)
        .produce(Symbol::Terminal::COMMA, 
                 Symbol::Nonterminal::PARAM,
                 Symbol::Nonterminal::PARAM_LIST_TAIL)
        
        .each(Symbol::Terminal::RPAREN)
        .produce();

    parseTable.from(Symbol::Nonterminal::PARAM)
        .each(Symbol::Terminal::ID)
        .produce(Symbol::Action::CreateVarDeclNode,
                 Symbol::Action::PushString,
                 Symbol::Action::AddIdToVar,
                 Symbol::Terminal::ID,
                 Symbol::Terminal::COLON,
                 Symbol::Nonterminal::TYPE,
                 Symbol::Action::AddParamToFunc);

    parseTable.from(Symbol::Nonterminal::RET_TYPE)
        .each(Symbol::Terminal::COLON)
        .produce(Symbol::Terminal::COLON,
                 Symbol::Nonterminal::TYPE)

        .each(Symbol::Terminal::BEGIN)
        .produce();

    //END DECLATIONS
    //BEGIN STATEMENTS

    parseTable.from(Symbol::Nonterminal::STAT_SEQ)
        .each(Symbol::Terminal::ID, 
              Symbol::Terminal::IF,
              Symbol::Terminal::WHILE, 
              Symbol::Terminal::FOR,
              Symbol::Terminal::BREAK, 
              Symbol::Terminal::RETURN,
              Symbol::Terminal::LET)
        .produce(Symbol::Nonterminal::STAT,
                 Symbol::Action::AddStatement,
                 Symbol::Action::StatementRecoveryCheckpoint,
                 Symbol::Nonterminal::STAT_SEQ_TAIL);

    parseTable.from(Symbol::Nonterminal::STAT_SEQ_TAIL)
        .each(Symbol::Terminal::ID, 
              Symbol::Terminal::IF,
              Symbol::Terminal::WHILE, 
              Symbol::Terminal::FOR,
              Symbol::Terminal::BREAK, 
              Symbol::Terminal::RETURN,
              Symbol::Terminal::LET)
        .produce(Symbol::Nonterminal::STAT_SEQ)

        .each(Symbol::Terminal::END, 
              Symbol::Terminal::ELSE,
              Symbol::Terminal::ENDIF, 
              Symbol::Terminal::ENDDO)
        .produce();

    parseTable.from(Symbol::Nonterminal::STAT)
        .each(Symbol::Terminal::ID)
        .produce(Symbol::Nonterminal::ASSIGN_OR_CALL,
                 Symbol::Terminal::SEMI)
                 
        .each(Symbol::Terminal::IF)
        .produce(Symbol::Terminal::IF,
                 Symbol::Nonterminal::EXPR,
                 Symbol::Action::CreateIfNode,
                 Symbol::Terminal::THEN,
                 Symbol::Action::CreateStmtSeqNode,
                 Symbol::Nonterminal::STAT_SEQ,
                 Symbol::Action::AddStmtSeqToIfBody,
                 Symbol::Nonterminal::IF_TAIL,
                 Symbol::Terminal::SEMI)
                 
        .each(Symbol::Terminal::WHILE)
        .produce(Symbol::Terminal::WHILE,
                 Symbol::Nonterminal::EXPR,
                 Symbol::Action::CreateWhileNode,
                 Symbol::Terminal::DO, 
                 Symbol::Nonterminal::STAT_SEQ,
                 Symbol::Terminal::ENDDO, 
                 Symbol::Terminal::SEMI)
                 
        .each(Symbol::Terminal::FOR)
        .produce(Symbol::Terminal::FOR,
                 Symbol::Action::PushString,
                 Symbol::Terminal::ID,
                 Symbol::Action::CreateForNode,
                 Symbol::Terminal::ASSIGN,
                 Symbol::Nonterminal::EXPR,
                 Symbol::Action::SetForStart,
                 Symbol::Terminal::TO,
                 Symbol::Nonterminal::EXPR,
                 Symbol::Action::SetForStop,
                 Symbol::Terminal::DO, 
                 Symbol::Nonterminal::STAT_SEQ,
                 Symbol::Terminal::ENDDO, 
                 Symbol::Terminal::SEMI)
                 
        .each(Symbol::Terminal::BREAK)
        .produce(Symbol::Terminal::BREAK,
                 Symbol::Action::CreateBreakNode,
                 Symbol::Terminal::SEMI)
                 
        .each(Symbol::Terminal::RETURN)
        .produce(Symbol::Terminal::RETURN,
                 Symbol::Nonterminal::EXPR,
                 Symbol::Action::CreateReturnNode,
                 Symbol::Terminal::SEMI)
                 
        .each(Symbol::Terminal::LET)
        .produce(Symbol::Terminal::LET,
                 Symbol::Action::CreateLetNode,
                 Symbol::Nonterminal::DECLARATION_SEGMENT, 
                 Symbol::Terminal::IN,
                 Symbol::Nonterminal::STAT_SEQ, 
                 Symbol::Terminal::END,
                 Symbol::Action::LetRecoveryCheckpoint);

    parseTable.from(Symbol::Nonterminal::ASSIGN_OR_CALL)
        .each(Symbol::Terminal::ID)
        .produce(Symbol::Action::PushString,
                 Symbol::Terminal::ID,
                 Symbol::Nonterminal::ASSIGN_OR_CALL_TAIL);

    parseTable.from(Symbol::Nonterminal::ASSIGN_OR_CALL_TAIL)
        .each(Symbol::Terminal::LPAREN)
        .produce(Symbol::Terminal::LPAREN,
                 Symbol::Action::CreateFuncCallNode,
                 Symbol::Nonterminal::EXPR_LIST,
                 Symbol::Terminal::RPAREN)

        .each(Symbol::Terminal::LBRACK,
              Symbol::Terminal::ASSIGN)
        .produce(Symbol::Nonterminal::LVALUE_TAIL,
                 Symbol::Terminal::ASSIGN,
                 Symbol::Nonterminal::EXPR_OR_CALL,
                 Symbol::Action::CreateAssignNode);

    parseTable.from(Symbol::Nonterminal::EXPR_OR_CALL)
        .each(Symbol::Terminal::LPAREN)
        .produce(Symbol::Terminal::LPAREN,
                 Symbol::Nonterminal::EXPR,
                 Symbol::Terminal::RPAREN,
                 Symbol::Nonterminal::EXPR_AFTER_FACTOR)

        .each(Symbol::Terminal::INTLIT,
              Symbol::Terminal::FLOATLIT)
        .produce(Symbol::Nonterminal::CONST,
                 Symbol::Nonterminal::EXPR_AFTER_FACTOR)

        .each(Symbol::Terminal::ID)
        .produce(Symbol::Action::PushString,
                 Symbol::Terminal::ID,
                 Symbol::Nonterminal::EXPR_OR_CALL_TAIL);

    parseTable.from(Symbol::Nonterminal::EXPR_OR_CALL_TAIL)
        .each(Symbol::Terminal::LPAREN)
        .produce(Symbol::Terminal::LPAREN,
                 Symbol::Action::CreateFuncCallNode,
                 Symbol::Nonterminal::EXPR_LIST,
                 Symbol::Terminal::RPAREN)

        .each(Symbol::Terminal::LBRACK,
              Symbol::Terminal::MULT,
              Symbol::Terminal::DIV,
              Symbol::Terminal::PLUS,
              Symbol::Terminal::MINUS,
              Symbol::Terminal::EQ,
              Symbol::Terminal::NEQ,
              Symbol::Terminal::LESSER,
              Symbol::Terminal::LESSEREQ,
              Symbol::Terminal::GREATER,
              Symbol::Terminal::GREATEREQ,
              Symbol::Terminal::AND,
              Symbol::Terminal::OR)
        .produce(Symbol::Nonterminal::LVALUE_TAIL,
                 Symbol::Nonterminal::EXPR_AFTER_FACTOR)

        .each(Symbol::Terminal::SEMI)
        .produce(Symbol::Action::CreateVariableAccess);

    parseTable.from(Symbol::Nonterminal::EXPR_AFTER_FACTOR)
        .each(Symbol::Terminal::MULT,
              Symbol::Terminal::DIV,
              Symbol::Terminal::PLUS,
              Symbol::Terminal::MINUS,
              Symbol::Terminal::EQ,
              Symbol::Terminal::NEQ,
              Symbol::Terminal::LESSER,
              Symbol::Terminal::LESSEREQ,
              Symbol::Terminal::GREATER,
              Symbol::Terminal::GREATEREQ,
              Symbol::Terminal::AND,
              Symbol::Terminal::OR)
        .produce(Symbol::Nonterminal::TERM_TAIL,
             Symbol::Nonterminal::COMPARAND_TAIL,
             Symbol::Nonterminal::LOGICAL_OPERAND_TAIL,
             Symbol::Nonterminal::EXPR_TAIL)

        .each(Symbol::Terminal::SEMI)
        .produce();

    parseTable.from(Symbol::Nonterminal::IF_TAIL)
        .each(Symbol::Terminal::ELSE)
        .produce(Symbol::Terminal::ELSE,
                 Symbol::Action::CreateStmtSeqNode,
                 Symbol::Nonterminal::STAT_SEQ,
                 Symbol::Action::AddStmtSeqToElse,
                 Symbol::Terminal::ENDIF)

        .each(Symbol::Terminal::ENDIF)
        .produce(Symbol::Terminal::ENDIF);

    parseTable.from(Symbol::Nonterminal::EXPR)
        .each(Symbol::Terminal::INTLIT,
              Symbol::Terminal::FLOATLIT,
              Symbol::Terminal::ID,
              Symbol::Terminal::LPAREN)
        .produce(Symbol::Nonterminal::LOGICAL_OPERAND,
                 Symbol::Nonterminal::EXPR_TAIL);

    parseTable.from(Symbol::Nonterminal::EXPR_TAIL)
        .each(Symbol::Terminal::AND,
              Symbol::Terminal::OR)
        .produce(Symbol::Action::PushString,
                 Symbol::Nonterminal::LOGICAL_OPERATOR,
                 Symbol::Nonterminal::LOGICAL_OPERAND,
                 Symbol::Action::CreateBinaryExpr,
                 Symbol::Nonterminal::EXPR_TAIL)

        .each(Symbol::Terminal::THEN,
              Symbol::Terminal::DO,
              Symbol::Terminal::TO,
              Symbol::Terminal::SEMI,
              Symbol::Terminal::RPAREN,
              Symbol::Terminal::COMMA,
              Symbol::Terminal::RBRACK)
        .produce();

    parseTable.from(Symbol::Nonterminal::LOGICAL_OPERATOR)
        .each(Symbol::Terminal::AND)
        .produce(Symbol::Terminal::AND)
        .each(Symbol::Terminal::OR)
        .produce(Symbol::Terminal::OR);

    parseTable.from(Symbol::Nonterminal::LOGICAL_OPERAND)
        .each(Symbol::Terminal::INTLIT,
              Symbol::Terminal::FLOATLIT,
              Symbol::Terminal::ID,
              Symbol::Terminal::LPAREN)
        .produce(Symbol::Nonterminal::COMPARAND,
                 Symbol::Nonterminal::LOGICAL_OPERAND_TAIL);

    parseTable.from(Symbol::Nonterminal::LOGICAL_OPERAND_TAIL)
        .each(Symbol::Terminal::EQ,
              Symbol::Terminal::NEQ,
              Symbol::Terminal::LESSER,
              Symbol::Terminal::LESSEREQ,
              Symbol::Terminal::GREATER,
              Symbol::Terminal::GREATEREQ)
        .produce(Symbol::Action::PushString,
                 Symbol::Nonterminal::COMPARATOR,
                 Symbol::Nonterminal::COMPARAND,
                 Symbol::Action::CreateBinaryExpr)

        .each(Symbol::Terminal::AND,
              Symbol::Terminal::OR,
              Symbol::Terminal::THEN,
              Symbol::Terminal::DO,
              Symbol::Terminal::TO,
              Symbol::Terminal::SEMI,
              Symbol::Terminal::RPAREN,
              Symbol::Terminal::COMMA,
              Symbol::Terminal::RBRACK)
        .produce();

    parseTable.from(Symbol::Nonterminal::COMPARATOR)
        .each(Symbol::Terminal::EQ)
        .produce(Symbol::Terminal::EQ)
        .each(Symbol::Terminal::NEQ)
        .produce(Symbol::Terminal::NEQ)
        .each(Symbol::Terminal::LESSER)
        .produce(Symbol::Terminal::LESSER)
        .each(Symbol::Terminal::LESSEREQ)
        .produce(Symbol::Terminal::LESSEREQ)
        .each(Symbol::Terminal::GREATER)
        .produce(Symbol::Terminal::GREATER)
        .each(Symbol::Terminal::GREATEREQ)
        .produce(Symbol::Terminal::GREATEREQ);

    parseTable.from(Symbol::Nonterminal::COMPARAND)
        .each(Symbol::Terminal::INTLIT,
              Symbol::Terminal::FLOATLIT,
              Symbol::Terminal::ID,
              Symbol::Terminal::LPAREN)
        .produce(Symbol::Nonterminal::TERM,
                 Symbol::Nonterminal::COMPARAND_TAIL);

    parseTable.from(Symbol::Nonterminal::COMPARAND_TAIL)
        .each(Symbol::Terminal::PLUS,
              Symbol::Terminal::MINUS)
        .produce(Symbol::Action::PushString,
                 Symbol::Nonterminal::PLUS_MINUS,
                 Symbol::Nonterminal::TERM,
                 Symbol::Action::CreateBinaryExpr,
                 Symbol::Nonterminal::COMPARAND_TAIL)

        .each(Symbol::Terminal::EQ,
              Symbol::Terminal::NEQ,
              Symbol::Terminal::LESSER,
              Symbol::Terminal::LESSEREQ,
              Symbol::Terminal::GREATER,
              Symbol::Terminal::GREATEREQ,
              Symbol::Terminal::AND,
              Symbol::Terminal::OR,
              Symbol::Terminal::THEN,
              Symbol::Terminal::DO,
              Symbol::Terminal::TO,
              Symbol::Terminal::SEMI,
              Symbol::Terminal::RPAREN,
              Symbol::Terminal::COMMA,
              Symbol::Terminal::RBRACK)
        .produce();

    parseTable.from(Symbol::Nonterminal::PLUS_MINUS)
        .each(Symbol::Terminal::PLUS)
        .produce(Symbol::Terminal::PLUS)
        .each(Symbol::Terminal::MINUS)
        .produce(Symbol::Terminal::MINUS);

    parseTable.from(Symbol::Nonterminal::TERM)
        .each(Symbol::Terminal::INTLIT,
              Symbol::Terminal::FLOATLIT,
              Symbol::Terminal::ID,
              Symbol::Terminal::LPAREN)
        .produce(Symbol::Nonterminal::FACTOR,
                 Symbol::Nonterminal::TERM_TAIL);

    parseTable.from(Symbol::Nonterminal::TERM_TAIL)
        .each(Symbol::Terminal::MULT,
              Symbol::Terminal::DIV)
        .produce(Symbol::Action::PushString,
                 Symbol::Nonterminal::MUL_DIV,
                 Symbol::Nonterminal::FACTOR,
                 Symbol::Action::CreateBinaryExpr,
                 Symbol::Nonterminal::TERM_TAIL)

        .each(Symbol::Terminal::PLUS,
              Symbol::Terminal::MINUS,
              Symbol::Terminal::EQ,
              Symbol::Terminal::NEQ,
              Symbol::Terminal::LESSER,
              Symbol::Terminal::LESSEREQ,
              Symbol::Terminal::GREATER,
              Symbol::Terminal::GREATEREQ,
              Symbol::Terminal::AND,
              Symbol::Terminal::OR,
              Symbol::Terminal::THEN,
              Symbol::Terminal::DO,
              Symbol::Terminal::TO,
              Symbol::Terminal::SEMI,
              Symbol::Terminal::RPAREN,
              Symbol::Terminal::COMMA,
              Symbol::Terminal::RBRACK)
        .produce();

    parseTable.from(Symbol::Nonterminal::MUL_DIV)
        .each(Symbol::Terminal::MULT)
        .produce(Symbol::Terminal::MULT)
        .each(Symbol::Terminal::DIV)
        .produce(Symbol::Terminal::DIV);

    parseTable.from(Symbol::Nonterminal::FACTOR)
        .each(Symbol::Terminal::INTLIT,
              Symbol::Terminal::FLOATLIT)
        .produce(Symbol::Nonterminal::CONST)

        .each(Symbol::Terminal::ID)
        .produce(Symbol::Nonterminal::LVALUE)

        .each(Symbol::Terminal::LPAREN)
        .produce(Symbol::Terminal::LPAREN,
                 Symbol::Nonterminal::EXPR,
                 Symbol::Terminal::RPAREN);

    parseTable.from(Symbol::Nonterminal::CONST)
        .each(Symbol::Terminal::INTLIT)
        .produce(Symbol::Action::PushInt,
                 Symbol::Terminal::INTLIT)

        .each(Symbol::Terminal::FLOATLIT)
        .produce(Symbol::Action::PushFloat,
                 Symbol::Terminal::FLOATLIT);

    parseTable.from(Symbol::Nonterminal::EXPR_LIST)
        .each(Symbol::Terminal::INTLIT,
              Symbol::Terminal::FLOATLIT,
              Symbol::Terminal::ID,
              Symbol::Terminal::LPAREN)
        .produce(Symbol::Nonterminal::EXPR,
                 Symbol::Action::AddExprToArguments,
                 Symbol::Nonterminal::EXPR_LIST_TAIL)

        .each(Symbol::Terminal::RPAREN)
        .produce();

    parseTable.from(Symbol::Nonterminal::EXPR_LIST_TAIL)
        .each(Symbol::Terminal::COMMA)
        .produce(Symbol::Terminal::COMMA,
                 Symbol::Nonterminal::EXPR_LIST_TAIL)

        .each(Symbol::Terminal::RPAREN)
        .produce();

    parseTable.from(Symbol::Nonterminal::LVALUE)
        .each(Symbol::Terminal::ID)
        .produce(Symbol::Action::PushString,
                 Symbol::Terminal::ID,
                 Symbol::Nonterminal::LVALUE_TAIL);

    parseTable.from(Symbol::Nonterminal::LVALUE_TAIL)
        .each(Symbol::Terminal::LBRACK)
        .produce(Symbol::Terminal::LBRACK,
                 Symbol::Nonterminal::EXPR,
                 Symbol::Action::CreateArrayAccess,
                 Symbol::Terminal::RBRACK)

        .each(Symbol::Terminal::ASSIGN,
              Symbol::Terminal::MULT,
              Symbol::Terminal::DIV,
              Symbol::Terminal::PLUS,
              Symbol::Terminal::MINUS,
              Symbol::Terminal::EQ,
              Symbol::Terminal::NEQ,
              Symbol::Terminal::LESSER,
              Symbol::Terminal::LESSEREQ,
              Symbol::Terminal::GREATER,
              Symbol::Terminal::GREATEREQ,
              Symbol::Terminal::AND,
              Symbol::Terminal::OR,
              Symbol::Terminal::THEN,
              Symbol::Terminal::DO,
              Symbol::Terminal::TO,
              Symbol::Terminal::SEMI,
              Symbol::Terminal::RPAREN,
              Symbol::Terminal::COMMA,
              Symbol::Terminal::RBRACK)
        .produce(Symbol::Action::CreateVariableAccess);
  }

  void error(std::string message) {
    numErrors++;
    std::cerr << globalFileName << ":" << scanner.getCurrLine()
              << ": error: " << message << "\n";
  }

  std::shared_ptr<AstNode> parse(Symbol start = Symbol::Nonterminal::TIGER_PROGRAM) {
    std::stack<Symbol>().swap(parseStack);
    parseStack.push(Symbol::Terminal::EOFF);
    parseStack.push(start);

    TokenPair word = advanceToken();
    std::string parseStackTemp = "";
    Symbol focus;

    std::vector<Symbol> expanded_rule;
    while (true) {
      focus = parseStack.top();
      if (focus == Symbol::Terminal::EOFF) {
        if (word.getSymbol() == focus) {
          break;
        } else {
          error("successful parse, but there are still more tokens");
        }
        break;
      }

      // Action
      if (focus.isAction()) {
        parseStack.pop();
        switch (focus.getAction()) {
          #include "../../gen-files/generated_symbol_to_action.inc"
        }
        continue;
      }

      if (word.getSymbol() == Symbol::Terminal::EOFF) {
        error("premature end of input, expecting " + focus.toString());
        break;
      }
      if (focus == word.getSymbol()) {
        parseStack.pop();
        word = advanceToken();
      } else if (parseTable.find(focus, word.getSymbol(), expanded_rule)) {
        parseStack.pop();
        for (auto iter = expanded_rule.rbegin(); iter != expanded_rule.rend();
             iter++) {
          parseStack.push(*iter);
        }
      } else {
        std::string errorMessage("expecting one of [");
        std::vector<Symbol> terminals = parseTable.findTerminals(focus);
        errorMessage += terminals[0].toString();
        for (int i = 1; i < terminals.size(); ++i) {
          errorMessage += ", " + terminals[i].toString();
        }
        errorMessage += "] and ";
        // If this is SEMI, wind back to the nearest recovery checkpoint.
        if (word.getSymbol() == Symbol::Terminal::SEMI) {
          errorMessage += "continued after ";
          while (!parseStack.empty() &&
                 parseStack.top() !=
                 Symbol::Action::StatementRecoveryCheckpoint &&
                 parseStack.top() != Symbol::Action::LetRecoveryCheckpoint) {
            parseStack.pop();
          }
        } else {
          errorMessage += "ignored ";
        }
        errorMessage += word.toString();
        error(errorMessage);
        if (parseStack.empty()) {
          break;
        }
        word = advanceToken();
      }
    }
    return popAst<AstNode>();
  }

private:
  TokenPair advanceToken() {
    currentToken = scanner.getToken();
    currentLine = scanner.getCurrLine();
    if (printDebug) {
      std::cout << currentToken.getSymbol().toShortString() << ' ';
    }
    return currentToken;
  }

  template<typename T>
  std::shared_ptr<T> popAst() {
    auto ret = std::dynamic_pointer_cast<T>(astStack.top()); astStack.pop();
    return ret;
  }

  void StatementRecoveryCheckpoint() {}
  void LetRecoveryCheckpoint() {}

  void PushString() {
    std::shared_ptr<StringNode> node(new StringNode);
    node->value = currentToken.getLexeme();
    node->line_number = currentLine;
    astStack.push(node);
  }
  void PushInt() {
    std::shared_ptr<IntNode> node(new IntNode);
    node->value = std::stoi(currentToken.getLexeme());
    node->line_number = currentLine;
    astStack.push(node);
  }
  void PushFloat() {
    std::shared_ptr<FloatNode> node(new FloatNode);
    node->value = std::stof(currentToken.getLexeme());
    node->line_number = scanner.getCurrLine();
    astStack.push(node);
  }

  void CreateModuleNode() {
    std::shared_ptr<ProgramNode> node(new ProgramNode);
    node->name = globalFileName;
    astStack.push(node);
  }

  void AddTypeToList() {
    std::shared_ptr<TypeDeclNode> type_node = popAst<TypeDeclNode>();
    std::shared_ptr<LetNode> let_node = popAst<LetNode>();
    let_node->types.push_back(type_node);
    astStack.push(let_node);
  }
  void CreateTypeDeclNode() {
    std::shared_ptr<TypeDeclNode> node(new TypeDeclNode);
    node->name = popAst<StringNode>();
    astStack.push(node);
  }
  void SetTypeArraySize() {
    std::shared_ptr<IntNode> size_node = popAst<IntNode>();
    std::shared_ptr<TypeInfoNode> type_node = popAst<TypeInfoNode>();
    type_node->array_size = size_node;
    astStack.push(type_node);
  }
  void SetTypeBaseType() {
    std::shared_ptr<StringNode> base_type = popAst<StringNode>();
    std::shared_ptr<TypeInfoNode> type_node = popAst<TypeInfoNode>();
    type_node->base_type = base_type;
    astStack.push(type_node);
  }

  void CreateVarDeclNode() {
    astStack.push(std::shared_ptr<VarDeclNode>(new VarDeclNode));
  }
  void AddIdToVar() {
    std::shared_ptr<StringNode> var_name = popAst<StringNode>();
    std::shared_ptr<VarDeclNode> var_decl = popAst<VarDeclNode>();
    var_decl->names.push_back(var_name);
    astStack.push(var_decl);
  }
  void SetVarInitValue() {
    std::shared_ptr<NumericLiteralNode> init_value = popAst<NumericLiteralNode>();
    std::shared_ptr<VarDeclNode> var_decl = popAst<VarDeclNode>();
    var_decl->init_value = init_value;
    astStack.push(var_decl);
  }
  void AddVarToList() {
    std::shared_ptr<VarDeclNode> var_decl = popAst<VarDeclNode>();
    std::shared_ptr<LetNode> let_node = popAst<LetNode>();
    let_node->vars.push_back(var_decl);
    astStack.push(let_node);
  }

  void CreateFuncDeclNode() {
    std::shared_ptr<FuncDeclNode> func_node(new FuncDeclNode);
    func_node->name = popAst<StringNode>();
    astStack.push(func_node);
  }
  void AddParamToFunc() {
    std::shared_ptr<VarDeclNode> var = popAst<VarDeclNode>();
    std::shared_ptr<FuncDeclNode> func = popAst<FuncDeclNode>();
    func->params.push_back(var);
    astStack.push(func);
  }
  void CreateTypeInfoNode() {
    std::shared_ptr<TypeInfoNode> node(new TypeInfoNode);
    astStack.push(node);
  }
  void SetFuncReturnType() {
    std::shared_ptr<TypeInfoNode> return_type = popAst<TypeInfoNode>();
    std::shared_ptr<FuncDeclNode> func_node = popAst<FuncDeclNode>();
    func_node->return_type = return_type;
    astStack.push(func_node);
  }
  void AddFuncToList() {
    std::shared_ptr<FuncDeclNode> func_decl = popAst<FuncDeclNode>();
    std::shared_ptr<LetNode> let_node = popAst<LetNode>();
    let_node->funcs.push_back(func_decl);
    astStack.push(let_node);
  }

  void CreateBinaryExpr() {
    std::shared_ptr<BinaryExprNode> node(new BinaryExprNode);
    node->right = popAst<ExprNode>();
    node->operator_ = popAst<StringNode>()->value;
    node->left = popAst<ExprNode>();
    astStack.push(node);
  }

  void CreateArrayAccess() {
    std::shared_ptr<ArrayAccessNode> node(new ArrayAccessNode);
    node->index = popAst<ExprNode>();
    node->name = popAst<StringNode>();
    astStack.push(node);
  }
  void CreateVariableAccess() {
    std::shared_ptr<VariableAccessNode> node(new VariableAccessNode);
    node->name = popAst<StringNode>();
    astStack.push(node);
  }

  void CreateFuncCallNode() {
    std::shared_ptr<FuncCallNode> node(new FuncCallNode);
    node->name = popAst<StringNode>();
    astStack.push(node);
  }
  void AddExprToArguments() {
    std::shared_ptr<ExprNode> expr = popAst<ExprNode>();
    std::shared_ptr<FuncCallNode> call = popAst<FuncCallNode>();
    call->arguments.push_back(expr);
    astStack.push(call);
  }

  void AddStatement() {
    std::shared_ptr<StatementNode> stmt = popAst<StatementNode>();
    std::shared_ptr<StmtSeqNode> with_statement_seq =
        popAst<StmtSeqNode>();
    with_statement_seq->stmts.push_back(stmt);
    astStack.push(with_statement_seq);
  }
  void CreateAssignNode() {
    std::shared_ptr<ExprNode> rhs = popAst<ExprNode>();
    std::shared_ptr<VariableAccessNode> lhs = popAst<VariableAccessNode>();
    std::shared_ptr<AssignNode> assign(new AssignNode);
    assign->target = lhs;
    assign->expression = rhs;
    astStack.push(assign);
  }

  void CreateStmtSeqNode() {
    astStack.push(std::shared_ptr<StmtSeqNode>(new StmtSeqNode));
  }

  void CreateIfNode() {
    std::shared_ptr<IfNode> node(new IfNode);
    node->condition = popAst<ExprNode>();
    astStack.push(node);
  }
  void AddStmtSeqToIfBody() {
    std::shared_ptr<StmtSeqNode> stmts = popAst<StmtSeqNode>();
    std::shared_ptr<IfNode> node = popAst<IfNode>();
    node->body = stmts;
    astStack.push(node);
  }
  void AddStmtSeqToElse() {
    std::shared_ptr<StmtSeqNode> stmts = popAst<StmtSeqNode>();
    std::shared_ptr<IfNode> node = popAst<IfNode>();
    node->else_ = stmts;
    astStack.push(node);
  }

  void CreateWhileNode() {
    std::shared_ptr<ExprNode> condition = popAst<ExprNode>();
    std::shared_ptr<WhileNode> node(new WhileNode);
    node->condition = condition;
    astStack.push(node);
  }

  void CreateForNode() {
    std::shared_ptr<StringNode> var = popAst<StringNode>();
    std::shared_ptr<ForNode> node(new ForNode);
    node->variable = var;
    astStack.push(node);
  }
  void SetForStart() {
    std::shared_ptr<ExprNode> start = popAst<ExprNode>();
    std::shared_ptr<ForNode> node = popAst<ForNode>();
    node->start = start;
    astStack.push(node);
  }
  void SetForStop() {
    std::shared_ptr<ExprNode> stop = popAst<ExprNode>();
    std::shared_ptr<ForNode> node = popAst<ForNode>();
    node->stop = stop;
    astStack.push(node);
  }

  void CreateBreakNode() {
    astStack.push(std::make_shared<BreakNode>());
  }

  void CreateReturnNode() {
    std::shared_ptr<ExprNode> expression = popAst<ExprNode>();
    std::shared_ptr<ReturnNode> node(new ReturnNode);
    node->expression = expression;
    astStack.push(node);
  }

  void CreateLetNode() {
    astStack.push(std::make_shared<LetNode>());
  }
};

/**
 * Prints helpful information for the user if they try to run the
 * program with a bad number of arguments
 */
void printHelp() {
  std::cout << "\n"
            << "You have entered an incorrect number of arguments."
            << "\n"
            << "\n"
            << "Please specify the file you wish to parse and"
            << "\n"
            << "optionally whether you want to flag for debugging."
            << "\n"
            << "\n"
            << "> parser <filename> -d"
            << "\n";
}
/**
 * There are two arguments: one mandatory and one optional
 *
 * The mandatory argument is the file to perform a parse on while the
 * optional flag -d indicates that the user wants to print debug info.,
 * namely the sequences of Tokens as the Scanner reads them
 */
int main(int argc, char** argv) {

  // The user has given us a bad number of args
  if (argc > 3 || argc < 2) {
    printHelp();
    return 0;
  }

  // Initialize the Parser with the given filename
  Parser      parser(argv[1]);



  // Print debug info. if flagged
  if (argc > 2 && strcmp(argv[2], "-d") == 0) {
    parser.printDebug       = true;
  }

  if (argc > 2 && strcmp(argv[2], "-d") != 0)
    std::cout << "\nPlease use \"-d\" as flag for debugging.\n";

  //================================//
  // Pass
  std::shared_ptr<AstNode> root = parser.parse();

  if (parser.numErrors > 0) {
    return 1;
  }

  if(parser.printDebug){
    DumpTreeVisitor dumper(std::cout);
    root->accept(dumper);
  }

  SymtabBuilder symtab_builder;
  root->accept(symtab_builder);

  if(parser.printDebug){
    symtab_builder.get_symtab().print(std::cout);
  }

  IrBuilder irBuilder(
    symtab_builder.get_symtab()
  );

  root->accept(irBuilder);

  auto           inst_vec = irBuilder.get_insns();

  std::string base_name = std::string(argv[1]);
  int pos = base_name.find_last_of('.');
  base_name = base_name.substr(0, pos);

  {
    NaiveRegisterAllocator reg_allocator;
    MipsBuilder mipsBuilder(
        inst_vec,
        symtab_builder.get_symtab(),
        reg_allocator.get_alloc_map()
    );
    std::string out_name = base_name + "_naive.s";
    std::ofstream out(out_name);
    mipsBuilder.build();
    for (auto insn : mipsBuilder.get_data()) {
      out << *insn << "\n";
    }
    for (auto insn : mipsBuilder.get_text()) {
      out << *insn << "\n";
    }
    out.close();
    std::cout << "Generated " << out_name << '\n';
  }


  InstructionMap inst_map;
  for (auto it = inst_vec.begin(); it != inst_vec.end(); ++it) {
    int index = std::distance(inst_vec.begin(), it);
    inst_map[*it] = index;
    //cout << index << "\t";
    //(*it)->print(std::cout);
    //std::cout << "\n";
  }


  CFG control_flow_graph(inst_vec, inst_map);

  {
    //IntraBlockRegisterAllocator reg_allocator;
    IntraBlockAllocationMap allocation_map(control_flow_graph, inst_map);
    //TestAllocationMap allocation_map(control_flow_graph);

    MipsBuilder mipsBuilder(
      irBuilder.get_insns(),
      symtab_builder.get_symtab(),
      allocation_map
    );

    mipsBuilder.build();
    std::string out_name = base_name + "_local.s";
    std::ofstream out(out_name);
    for (auto insn : mipsBuilder.get_data()) {
      out << *insn << "\n";
    }
    for (auto insn : mipsBuilder.get_text()) {
      out << *insn << "\n";
    }
    out.close();
    std::cout << "Generated " << out_name << '\n';
  }


  //std::cout << "successful parse\n";
  return 0;
}
