/* run.config*
   STDOPT: #" -eva-mlevel 0 -eva-no-alloc-returns-null"
*/

extern int i;

#include "stdlib.h"


void main() {
  int size1, size2;
  size1 = (int) (&size1 + i);
  size2 = i + ((int)&size2 >> 1);
  int *p = malloc((unsigned long)&i+(int)&i);
  int *q = malloc(size1);
  int *r = malloc(size2);

  Frama_C_show_each(p, q, r);
  Frama_C_show_each(p+(int)p);

  *p = (int) (p+1);
  *q = (int) (q+2);
  *r = (int) (r+3);

  free(p+(int)p);
  free(q+(int)r);
}
