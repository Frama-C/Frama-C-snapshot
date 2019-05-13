/* run.config*
   OPT: -eva @EVA_CONFIG@ -eva-mlevel 3
   OPT: -eva @EVA_CONFIG@ -eva-malloc-functions my_calloc
*/

#include <stdlib.h>


void* my_calloc (int s, int n) {
  return malloc (s*n);
}

volatile foo;

void main () {
  char *p1 = my_calloc (0, 0);
  if (p1 == 0) Frama_C_show_each_NULL_p1();

  if (p1)
    Frama_C_show_each_not_NULL_p1();
  else
    Frama_C_show_each_NULL_p1();

  if (p1+1 == 0) Frama_C_show_each_NULL_p1_plus1(); // emit pointer comparable

  char *q1;
  while (foo) {
    if (foo) {
      q1 = my_calloc (0, 0);

    }
  }
  Frama_C_dump_each();
  if (foo) {
    *p1 = 1;
  }
  free (p1);

  Frama_C_show_each(q1);
  //@ assert !\initialized(q1) || !\valid (q1);
  if (foo) {
    *q1 = 1;
  }
  free (q1);
}
