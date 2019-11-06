/* run.config
   COMMENT: initialized and function calls
*/

#include "stdlib.h"

extern void *malloc(size_t);

int *A, *B;

void f() {
  A = B;
}

void g(int *C, int* D) {
  /*@ assert \initialized(&C); */
}

int main(void) {
  int *x, *y;
  B = (int*) malloc(sizeof(int));
  y = (int*) malloc(sizeof(int));
  x = y; 
  f();
  /*@ assert \initialized(&A); */
  /*@ assert \initialized(&x); */
  g(x, y);
  return 0;
}
