#include <math.h>

const double pi = 3.14159265358979323846264338327950288;
const double half_pi = 1.57079632679489661923132169163975144;
const double e = 2.718281828459045090795598298427648842334747314453125;
volatile double top;
const float f_pi = 3.14159265358979323846264338327950288F;
const float f_half_pi = 1.57079632679489661923132169163975144F;
const float f_e = 2.718281828459045090795598298427648842334747314453125F;
volatile float f_top;
const long double ld_pi = 3.14159265358979323846264338327950288L;
const long double ld_half_pi = 1.57079632679489661923132169163975144L;
const long double ld_e = 2.718281828459045090795598298427648842334747314453125L;
volatile long double ld_top;
const double zero = 0.0;
const double minus_zero = -0.0;
const double one = 1.0;
const double minus_one = -1.0;
const double large = 1e38;

#define TEST_VAL(type,f,c) type f##_##c = f(c)

#define TEST_FUN(type,f,prefix)                 \
  TEST_VAL(type,f,prefix##pi);                  \
  TEST_VAL(type,f,prefix##half_pi);             \
  TEST_VAL(type,f,prefix##e);                   \
  TEST_VAL(type,f,zero);                        \
  TEST_VAL(type,f,minus_zero);                  \
  TEST_VAL(type,f,one);                         \
  TEST_VAL(type,f,minus_one);                   \
  TEST_VAL(type,f,large);                       \
  TEST_VAL(type,f,prefix##top)

int main() {
  TEST_FUN(double,atan,);
  TEST_FUN(float,atanf,f_);
  TEST_FUN(long double,atanl,ld_);
  TEST_FUN(double,fabs,);
  TEST_FUN(float,fabsf,f_);
  TEST_FUN(long double,fabsl,ld_);
}
