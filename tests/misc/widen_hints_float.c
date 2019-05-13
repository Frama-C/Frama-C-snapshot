/* run.config*
   STDOPT: #"-val-subdivide-non-linear 20"
*/


#include "__fc_builtin.h"
#include <math.h>

void parabola(void) {
  /*
    The expression is a parabola p
      where p([0.;64.]) = [0.;64.] and p([64.;128.]) = [0.;64.].
      For any value x<0, p(x) < x;
      For any value 128.<x, p(x) < -x;
   */

  double f1 = Frama_C_double_interval(0.,1./64.);

  //@ loop widen_hints f1, 71.;
  for(int i = 0; i < 100; i++){
    f1 = (64*64 - (f1 - 64) * (f1 - 64))/64;
  }

  double f2 = Frama_C_double_interval(-1./64.,-0);

  //@ loop widen_hints f2, -80.;
  for(int i = 0; i < 100; i++){
    f2 = -(64*64 - (-f2 - 64) * (-f2 - 64))/64;
  }

  double f3 = Frama_C_double_interval(0.,1./64.);

  for(int i = 0; i < 100; i++){
    f3 = (64*64 - (f3 - 64) * (f3 - 64))/64;
  }
}

void trigo(void) {
  double f1 = 0.0;

  for (int i = 0; i < 100; i++) {
    f1 = sin(f1 + Frama_C_double_interval(-0.01, 0.01));
  }
}

void first_order_filter(void) {
  float f1 = 0.0;
  for (int i = 1; i < 100; i++)
    f1 = f1 * 0.8 + Frama_C_double_interval(-1.0, 1.0);

  float f2 = 0.0;
  //@ loop widen_hints f2, 5., -5.;
  for (int i = 1; i < 100; i++)
    f2 = f2 * 0.8 + Frama_C_double_interval(-1.0, 1.0);
}

void newton_sqrt(void) {
  double f1 = 2.0;
  for (int i = 1; i < 100; i++)
    f1 = (f1 + 2.0 / f1) / 2.0;

  double f2 = 2.0;
  //@ loop widen_hints f2, 1.4;
  for (int i = 1; i < 100; i++) {
    f2 = (f2 + 2.0 / f2) / 2.0;
  }
}

void main(voi) {
  parabola();
  trigo();
  first_order_filter();
  newton_sqrt();
}
