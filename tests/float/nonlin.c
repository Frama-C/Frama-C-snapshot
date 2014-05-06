/* run.config
   OPT: -slevel 30 -val -cpp-command "gcc -C -E -DFLOAT=double -I. " share/builtin.c -float-hex -journal-disable -subdivide-float-var 0
   OPT: -slevel 30 -val -cpp-command "gcc -C -E -DFLOAT=double -I. " share/builtin.c -float-hex -journal-disable -subdivide-float-var 10
   OPT: -slevel 30 -val -cpp-command "gcc -C -E -DFLOAT=float -I. " share/builtin.c -float-hex -journal-disable -subdivide-float-var 0
   OPT: -slevel 30 -val -cpp-command "gcc -C -E -DFLOAT=float -I. " share/builtin.c -float-hex -journal-disable -subdivide-float-var 10
*/

#include "share/builtin.h"

FLOAT a, b, c, r1, r2, d, i, s, zf, s2, sq, h;

int t[10]={1,2,3,4,5,6,7,8,9,10},r,z;

void nonlin_f()
{
  a = Frama_C_float_interval(5.0, 7.0);
  b = Frama_C_float_interval(0.0, 1.0);
  c = 7.0;
  d = a;
  /*@ assert (5.0 <= d) ; */

  r1 = a + (b * (c - a));
  /*@ assert
       (5.0 <= a <= 5.125)
    || (5.125 <= a <= 5.25)
    || (5.25 <= a <= 5.375)
    || (5.375 <= a <= 5.5)
    || (5.5 <= a <= 5.625)
    || (5.625 <= a <= 5.75)
    || (5.75 <= a <= 5.875)
    || (5.875 <= a <= 6.0)
    || (6.0 <= a <= 6.125)
    || (6.125 <= a <= 6.25)
    || (6.25 <= a <= 6.375)
    || (6.375 <= a <= 6.5)
    || (6.5 <= a <= 6.625)
    || (6.625 <= a <= 6.75)
    || (6.75 <= a <= 6.875)
    || (6.875 <= a <= 7.0) ; */

  r2 = a + (b * (c - a));
  Frama_C_show_each_a_r2("a", a, "r2", r2);
}

unsigned long rbits1;
int rbits2;

int access_bits(FLOAT X )
{  unsigned long x0;
  x0 = *((unsigned long *)(& X));
  if (x0 > 2UL) return 1;
  rbits1 = x0;
  return 0;
}


main()
{
  nonlin_f();

  i = Frama_C_float_interval(-133.0,142.0);
  s = Frama_C_float_interval(-133.0,142.0);
  r = 1 + t[(int)(i*i+2.0)];
  z = (int)(10000.0 * (s - s));
  zf = s - s;
  s2 = s + s;
  sq = s * s;
  h = s * (1 - s);
  rbits2 = access_bits(i);
}
