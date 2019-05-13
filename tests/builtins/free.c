/* run.config*
   STDOPT: #" -eva-builtin malloc:Frama_C_malloc_fresh"
*/
#include "stdlib.h" 
volatile v;

void main1() {
  int *p = malloc(40);
  p[1] = 1;
  int *q = malloc(40);
  q[2] = 2;
  int *r = v ? p : q;
  Frama_C_dump_each();
  free(r);

  int *u = malloc(40);
  u[3] = 3;
  free(u);

  int* t = 0;
  free(t);

  int* s = malloc(40);
  s[4] = 4;
  s = v ? 0 : s;
  free(s);
}



void main2() {
  int *p;
  int i = 1;

  p = malloc(i * sizeof (int));
  if (p != 0) {
    *p = i;
  }
  free(p); /* we must not backward-propagate information about p
                      before and after the call, because it became dangling. */
}

void main() {
  if (v) main1 ();
  else if (v) main2();
}
