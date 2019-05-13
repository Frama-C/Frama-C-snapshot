/* run.config*
   OPT: -eva @EVA_CONFIG@ -then -main main_log_exp
*/

#include <__fc_builtin.h>
#include "math.h"

void main(int c, char **v)
{
  float f;
  double d;
  f = Frama_C_float_interval(-1.0, 1.0);
  d = Frama_C_double_interval(-1.0, 1.0);
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
  if (v) {

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
    e6 = exp(0x1.62e42fefa39efp9);
  }
  if (v) {
    l7 = log((double)(int)&d);
  }
  if (v) {
    int x;
    l8 = log(x);
  }
}
