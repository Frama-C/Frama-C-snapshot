/* run.config
   COMMENT: \valid in presence of aliasing
*/

#include "stdlib.h"

int main(void) {
  int *a, *b, n = 0;
  /*@ assert ! \valid(a) && ! \valid(b); */
  a = malloc(sizeof(int));
  *a = n;
  b = a;
  /*@ assert \valid(a) && \valid(b); */
  /*@ assert *b == n; */
  free(b);
  /*@ assert ! \valid(a) && ! \valid(b); */
  return 0;
}
