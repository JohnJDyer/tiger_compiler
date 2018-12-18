#include "../common/Symbol.hpp"
#include <iostream>
#include <fstream>

using namespace std;

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " " << "<destination>\n";
    return 1;
  }
  ofstream f(argv[1]);
  int first = static_cast<int>(Symbol::Action::__FIRST) + 1;
  int last = static_cast<int>(Symbol::Action::__LAST);
  Symbol action(first);
  do {
    string action_name = action.toString().substr(8);
    f << "case Symbol::Action::" << action_name << ": " << action_name << "(); break;\n";
    action = Symbol(static_cast<int>(action.getAction()) + 1);
  } while (action < last);
  f << "default: throw std::exception();";
  return 0;
}
