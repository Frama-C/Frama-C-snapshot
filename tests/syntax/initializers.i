struct e { int i1; };

void f() {
  struct e e1 = {1, 2}; // accepted by GCC (with warning)
}

struct ee { int i2; };

void g() {
  struct ee e2 = 1; // not accepted by GCC; should at least emit warning
}
