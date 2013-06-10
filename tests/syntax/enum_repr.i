/* run.config
EXECNOW: make -s tests/syntax/Enum_repr.cmxs
OPT: -load-module tests/syntax/Enum_repr.cmxs -enums int -print -check
OPT: -load-module tests/syntax/Enum_repr.cmxs -enums gcc-short-enums -print -check
OPT: -load-module tests/syntax/Enum_repr.cmxs -enums gcc-enums -print -check
*/
typedef enum { A = 3 } foo;
typedef enum __attribute__((packed)) { B = 6 } bar;

int main () {
  foo x = A;
  bar y = B;
  if (x==A && y == B) { return 0; } return 1;
}

typedef unsigned int bla;

int f(bla x);

int g() {
  foo x = A;
  int res = f((bla) x);
  res+= f((unsigned int) x);
  res+= f(x);
  return res;
}
