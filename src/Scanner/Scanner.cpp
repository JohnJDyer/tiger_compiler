#include "Scanner.hpp"

/**
 * For testing purposes
 */
int main(int argc, char **argv) {
  Scanner scanner(argv[1]);
  TokenPair tokenPair = scanner.getToken();
  while (tokenPair.getSymbol() != Symbol::Terminal::EOFF) {
    std::cout << tokenPair.toString();
    tokenPair = scanner.getToken();
  }
  return 0;
}
