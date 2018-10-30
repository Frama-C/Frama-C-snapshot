/* run.config
   STDOPT: #"-warn-special-float none"
 */
#include <math.h>

volatile int v;

void main() {
  double d;
  float f;
  int classd, classf;
  char is_nan, is_inf, is_normal;
  d = v ? 0.1 : 1e307;
  classd = fpclassify(d);
  //@ assert classd == FP_NORMAL;
  is_nan = isnan(d);
  //@ assert is_nan == 0;
  is_inf = isinf(d);
  //@ assert is_inf == 0;
  is_normal = isnormal(d);
  //@ assert is_normal != 0;

  d = v ? -0.0 : 0.0;
  classd = fpclassify(d);
  //@ assert classd == FP_ZERO;
  is_normal = isnormal(d);
  //@ assert is_normal == 0;

  classd = fpclassify(1e999);
  //@ assert classd == FP_INFINITE;
  is_nan = isnan(1e999);
  //@ assert is_nan == 0;
  is_inf = isinf(1e999);
  //@ assert is_inf != 0;
  is_normal = isnormal(1e999);
  //@ assert is_normal == 0;

  d = v ? 1e-320 : 1e-310;
  classd = fpclassify(d);
  //@ assert classd == FP_SUBNORMAL;
  is_normal = isnormal(d);
  //@ assert is_normal == 0;

  classd = fpclassify(0.0/0.0);
  //@ assert classd == FP_NAN;
  is_nan = isnan(0.0/0.0);
  //@ assert is_nan != 0;

  classf = fpclassify(1e-40f);
  //@ assert classf == FP_SUBNORMAL;
  is_normal = isnormal(1e-40f);
  //@ assert is_normal == 0;

  classf = fpclassify(1e40f);
  //@ assert classf == FP_INFINITE;
  is_inf = isinf(1e40f);
  //@ assert is_inf != 0;
  is_normal = isnormal(1e40f);
  //@ assert is_normal == 0;

  d = v ? -1e-309 : 0.0;
  classd = fpclassify(d);
  //@ assert classd == FP_ZERO || classd == FP_SUBNORMAL;

  f = v ? 1e37f : 1e40f;
  classf = fpclassify(f);
  //@ assert classf == FP_NORMAL || classf == FP_INFINITE;

}
