#include <stdlib.h>

volatile int nondet;

int *f(int i) {
  int a[i];
  if (nondet) free(a); // must fail
  return a; // will become dangling
}

void main() {
  int *t = f(4);
  free(t);
}
