/* run.config*
   STDOPT: #" -val-mlevel 0 -no-val-alloc-returns-null"
*/

extern int i;

#include "stdlib.h"


void main() {
  int size1, size2;
  size1 = &size1 + i;
  size2 = i + ((int)&size2 >> 1);
  int *p = malloc((unsigned long)&i+(int)&i);
  int *q = malloc(size1);
  int *r = malloc(size2);

  Frama_C_show_each(p, q, r);
  Frama_C_show_each(p+(int)p);

  *p = p+1;
  *q = q+2;
  *r = r+3;

  free(p+(int)p);
  free(q+(int)r);
}
