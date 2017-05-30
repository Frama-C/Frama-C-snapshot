/* run.config*
   STDOPT: +"-val-subdivide-non-linear 14 -value-msg-key nonlin"
*/

#include "__fc_builtin.h"

volatile int v; volatile short vs;

/* Checks that the subdivision does not fail when pointer values get involved. */
void subdivide_pointer () {
  int y, x = 17;
  int *p = &x;
  int i = Frama_C_interval(0,100);

  /* The complete expression is a pointer. */
  int *q = p + i - i;

  /* The complete expression is an integer, but the subterm on which the
     subdivision is done is a pointer. */
  y = *(p + i - i);

  /* The splitted lvalue contains a pointer value. */
  i = v ? i : &x;
  y = *(p + i - i);
}


void subdivide_integer () {
  int y;
  short z = v;
  int k = (z+675) * (z+675);
  int l = (z+17817) * (z+17817);

  int x = sizeof(y)+sizeof(y); // do not optimize y
  int *p = &x + x; // do not optmize x;

  long long i1 = vs;
  long long i2 = vs;
  long long r = i1 * i1 + (i2+3) * (i2+3); // (i2+3) not fully precise with 14 subdivisions

  int t[102];
  short idx = vs;
  //@ assert 0 <= idx <= 10;
  t[idx*idx] = 1;
}

void main () {
  subdivide_integer ();
  subdivide_pointer ();
}
