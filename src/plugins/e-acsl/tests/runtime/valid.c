/* run.config
   COMMENT: \valid
   STDOPT: +"-no-val-alloc-returns-null"
*/

#include "stdlib.h"

extern void *malloc(size_t p);
extern void free(void* p);

int *X, Z;

/*@ requires \valid(x);
  @ ensures \valid(\result); */
int *f(int *x) {
  int *y;
  /*@ assert ! \valid(y); */
  y = x;
  /*@ assert \valid(x); */
  return y;
}

void g(void) {
  int m, *u, **p;
  p=&u;
  u=&m;
  m=123;
  //@ assert \valid(*p);
}

int main(void) {
  int *a, *b, **c, ***d, n = 0;
  /*@ assert ! \valid(a) && ! \valid(b) && ! \valid(X); */
  a = malloc(sizeof(int));
  /*@ assert \valid(a) && ! \valid(b) && ! \valid(X); */
  X = a;
  /*@ assert \valid(a) && ! \valid(b) && \valid(X); */
  b = f(&n);
  /*@ assert \valid(a) && \valid(b) && \valid(X); */
  X = b;
  /*@ assert \valid(a) && \valid(b) && \valid(X); */
  c = &a;
  d = &c;
  /*@ assert \valid(*c); */
  /*@ assert \valid(**d); */
  free(a);
  /*@ assert ! \valid(a) && \valid(b) && \valid(X); */
  /*@ assert \valid(&Z); */
  g();
  return 0;
}
