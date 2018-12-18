/**
 * TokenPair: the pair of <TokenType, String> returned from Scanner's getToken
 *
 * @author:  gatech Buzz
 */
#pragma once
#include "Symbol.hpp"

class TokenPair {
 private:
  Symbol symbol;
  std::string lexeme;

 public:
  TokenPair() : TokenPair(Symbol::Terminal::EOFF, "") {}

  TokenPair(Symbol symbol, std::string lexeme) {
    this->symbol = symbol;
    this->lexeme = lexeme;
  }

  Symbol getSymbol() const {
    return symbol;
  }

  std::string getLexeme() const {
    return lexeme;
  }

  std::string toString() const {
    return std::string("<") + symbol.toString() + ", \"" + lexeme + "\">";

  }
};
