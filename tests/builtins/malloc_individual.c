/* run.config*
   STDOPT: #"-eva-no-builtins-auto"
*/
#define malloc(n) Frama_C_malloc_fresh(n)
#include "share/libc/stdlib.c"

int *p;
int A,B,C;

void main(int c)
{
  p = malloc(sizeof(int));
  if (c)
    *p = 3;
  A = *p;
  C = 1 + *p;
  B = A;
}
