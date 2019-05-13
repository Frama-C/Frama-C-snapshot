/* run.config*
   OPT: -eva-msg-key nonlin -slevel 30 -eva @EVA_CONFIG@ -cpp-extra-args="-DFLOAT=double" -float-hex -journal-disable -eva-subdivide-non-linear 0
   OPT: -eva-msg-key nonlin -slevel 30 -eva @EVA_CONFIG@ -cpp-extra-args="-DFLOAT=double" -float-hex -journal-disable -eva-subdivide-non-linear 10
   OPT: -eva-msg-key nonlin -slevel 30 -eva @EVA_CONFIG@ -cpp-extra-args="-DFLOAT=float" -float-hex -journal-disable -eva-subdivide-non-linear 0
   OPT: -eva-msg-key nonlin -slevel 30 -eva @EVA_CONFIG@ -cpp-extra-args="-DFLOAT=float" -float-hex -journal-disable -eva-subdivide-non-linear 10
*/

#include "__fc_builtin.h"

FLOAT a, b, c, r1, r2, d, i, s, zf, s2, sq, h;

int t[10]={1,2,3,4,5,6,7,8,9,10},r,x,y,z;

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

  r2 = (b * (c - a)) + a;
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

volatile float v;

void other() {
  i = Frama_C_float_interval(-133.0,142.0);
  s = Frama_C_float_interval(-133.0,142.0);
  r = 1 + t[(int)(i*i+2.0)];
  z = (int)(10000.0 * (s - s));
  zf = s - s;
  s2 = s + s;
  sq = s * s;
  h = s * (1 - s);
  rbits2 = access_bits(i);

  x = Frama_C_interval(0,42);
  y = (1 / x) * x;
}

void split_alarm() { // No alarm with sufficient subdivide-float-var
  float ff = v;
  double d = 1 / ((double)ff * ff + 0.000000001);
}

void norm() {
  float v1 = v;
  float v2 = v;
  double square = (double)v1*v1+(double)v2*v2;
}

// a bug resulted in an invalid interval due to the presence of garbled mix
void garbled() {
  int x;
  float a = (float)((int)(&x + (int)&x));
  float f = a + a;
}

// Tests possible bugs of the subdivision around the zero values.
void around_zeros() {
  /* [f1] is the smallest positive float, and [f] has values in [-0 .. f1].
     While [next_float_ieee -0. = f1], the interval [-0. .. f1] contains three
     float values, so its subdivision should not forget +0. */
  float f1 = 1.4E-45;
  float f = Frama_C_float_interval(-0, f1);
  /* The +f-f is needed to activate the subdivisions.
     The [f1] value is removed from [f], which must become [-0. .. 0.]
     and not the singleton {-0.}.  */
  float res = f1 / (f+f-f - f1);
}

void main() {
  nonlin_f();
  other ();
  split_alarm();
  norm();
  garbled();
  around_zeros();
}
