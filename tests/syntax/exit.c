#include <stdlib.h>

volatile c;

int f() {
  if (c) {
    return 0;
  } else {
    quick_exit (1); // no_return attribute in libc
  }
}

void unknown_exit(int i);

int g() {
  if (c) {
    return 0;
  } else {
    unknown_exit (1);
  }
}


void main() {
  f();
  g();
}
