/* run.config
   COMMENT: Check handling function definitions with pointer parameters
*/

#include <stdint.h>
#include <stdlib.h>

int* pfun(char c, int *p, int *p2, int *p3, int *p4, int i) {
  int *q = p;
  return q;
}

int main(int argc, const char **argv) {
  int a = 1;
  int *p = &a;
  uintptr_t addr = (uintptr_t)&addr;

  /* Function with definition returning a pointer:
     - save all pointer parameters
        + call via pointer - save referent number
        + call via addressof - sabe block number
        + call via NULL - save invalid
        + call via value - save block number
     - store return referent in return handler
     - take referent number of the return value (via return handler) */
  p = pfun('a', p, &a, NULL, (int*)addr, 2);

  /*@assert \valid(p); */
  return 0;
}


