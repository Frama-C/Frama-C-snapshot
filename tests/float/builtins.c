/* run.config
   OPT: -val -then -main main_log_exp -then -all-rounding-modes
*/

#include <__fc_builtin.h>

void main(int c, char **v)
{
  float f;
  double d;
  f = Frama_C_float_interval(-1.0, 1.0);
  d = Frama_C_double_interval(-1.0, 1.0);
}

/*@ assigns \result \from d; */
double Frama_C_log(double d);
/*@ assigns \result \from d; */
double Frama_C_log10(double d);
/*@ assigns \result \from d; */
double Frama_C_exp(double d);

/*@ requires \is_finite(d);
  requires d > 0.;
  ensures \is_finite(d); */
double log(double d) {
  return Frama_C_log(d);
}

/*@ requires \is_finite(d);
  requires d > 0.;
  ensures \is_finite(d); */
double log10(double d) {
  return Frama_C_log10(d);
}

/*@ requires \is_finite(d);
   requires d <= 0x1.62e42fefa39efp9; // log(DBL_MAX)
   ensures \is_finite(d);
*/
double exp(double d) {
  return Frama_C_exp(d);
}

volatile v;

void main_log_exp(double d) {
  double l1, l2, l3, l4, l5, l6, l7, l8;
  double m1, m2, m3, m4, m5, m6;
  double e1, e2, e3, e4, e5, e6;
  if (v) {
    l1 = log(1);
    m1 = log10(1.);
  }
  if (v) {
    l2 = log(2.72);
    m2 = log10(10.);
  }
  if (v) {
    l3 = log(3.);
    m3 = log10(20.);
  }
  if (v) {
    //@ assert 10 <= d <= 100;
    l4 = log(d);
  }
  if (v) {
    //@ assert 10 <= d <= 101;
    m4 = log10(d);
  }
  if (v) { // Spurious warning in -all-rounding-modes, because the reduction
           // to >0. is transformed in >=0.
    l5 = log(d);
  }
  if (v) {
    m5 = log10(d); // Same
  }
  if (v) {
    l6 = log(-1);
  }
  if (v) {
    m6 = log10(-1);
  }

  if (v) {
    e1 = exp(0.);
  }
  if (v) {
    e2 = exp(-0x1.fffffffffffffp+1023);
  }
  if (v) {
    e3 = exp(100);
  }
  if (v) {
    //@ assert 100 <= d <= 1000;
    e4 = exp(d);
  }
  if (v) {
    e5 = exp(1000);
  }
  if (v) {
    double max1 = 0x1.62e42fefa39efp9;
    double max2 = log(0x1.fffffffffffffp+1023);
    //@ assert max1 == max2;
    e6 = Frama_C_exp(0x1.62e42fefa39efp9);
  }
  if (v) {
    l7 = log((double)(int)&d);
  }
  if (v) {
    int x;
    l8 = log(x); // indeterminate caught by Value before call, AND verified by
                 // builtin
  }
}
