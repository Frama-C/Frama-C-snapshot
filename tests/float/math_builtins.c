/* run.config
  FILTER: sed -e '/f32__/ s/\([0-9][.][0-9]\{6\}\)[0-9]\{10\}/\1/g'
  COMMENT: 'sed' filter is a temporary workaround due to libc imprecisions
  STDOPT: #"-float-normal -val -val-builtin sqrt:Frama_C_sqrt,exp:Frama_C_exp,log:Frama_C_log,log10:Frama_C_log10,cos:Frama_C_cos,sin:Frama_C_sin,atan2:Frama_C_atan2,pow:Frama_C_pow,fmod:Frama_C_fmod,sqrtf:Frama_C_sqrtf,expf:Frama_C_expf,logf:Frama_C_logf,log10f:Frama_C_log10f,powf:Frama_C_powf,floor:Frama_C_floor,ceil:Frama_C_ceil,trunc:Frama_C_trunc,round:Frama_C_round,floorf:Frama_C_floorf,ceilf:Frama_C_ceilf,truncf:Frama_C_truncf,roundf:Frama_C_roundf -then -no-val-print -val-print"
*/ 
#include <math.h>

static volatile int nondet;
#define assert_bottom(exp) if (nondet) { exp; Frama_C_show_each_unreachable(); }

double double_interval(double min, double max) {
  if (nondet) return min;
  else return max;
}

void test_cos_det() {
  double x = cos(1.);
  double y = cos(0.);
  double z = cos(-1.);
}

void test_sin_det() {
  double x = sin(1.);
  double y = sin(0.);
  double z = sin(-1.);
}

void test_atan2_det() {
  double a = atan2(1.,0.);
  double b = atan2(0.,1.);
  double c = atan2(1.,-0.);
  double d = atan2(-0.,1.);
  double e = atan2(-1.,0.);
  double f = atan2(-1.,0.);
  double g = atan2(-1.,-0.);
  double h = atan2(-1.,-0.);
  double i = atan2(0.,0.);
  double j = atan2(2.,1.);
  double k = atan2(-2.,1.);
  double l = atan2(2.,-1.);
  double m = atan2(-2.,-1.);
}

void test_atan2() {
  double x, y;
  y = double_interval(0.125, 10.);
  x = double_interval(0.125, 10.);
  double a = atan2(y, x);
  x = double_interval(-2.5, 3.);
  double b = atan2(y, x);
  y = double_interval(-1.25, -0.5);
  double c = atan2(y, x);
  x = double_interval(-2.5, -3.);
  double d = atan2(y, x);
  x = double_interval(-0., +0.);
  double e = atan2(y, x);
  x = double_interval(1., 5.);
  y = double_interval(-1., -0.);
  double f = atan2(y, x);
  y = double_interval(0., 0.5);
  double g = atan2(y, x);
  y = double_interval(-0., 0.5);
  double h = atan2(y, x);
  y = double_interval(-0.5, 0.);
  double i = atan2(y, x);
  y = double_interval(-0., 0.);
  double j = atan2(y, x);
  double k = atan2(0., x);
  double l = atan2(-0., x);
  double m = atan2(0., -x);
  double n = atan2(-0., -x);
}

void test_pow_det() {
  double a = pow(1.,8.);
  double b = pow(0.,1.);
  double c = pow(12.,0.);
  double d = pow(2.25,0.25);
  double e = pow(178.25,0.25);
  double f = pow(129.5,-0.25);
  double g = pow(512.,-2.25);
  double h = pow(0.,0.);
}

void test_powf_det() {
  float f32__a = powf(1.,8.);
  float f32__b = powf(0.,1.);
  float f32__c = powf(12.,0.);
  float f32__d = powf(2.25,0.25);
  float f32__e = powf(178.25,0.25);
  float f32__f = powf(129.5,-0.25);
  float f32__g = powf(512.,-2.25);
  float f32__h = powf(0.,0.);
}

void test_pow_singleton_exp() {
  double x;
  x = double_interval(-3.5, -2.75);
  // error: x contains only negative values, and y is a non-integer
  assert_bottom(pow(x, 1.5));
  // again, but with negative y
  assert_bottom(pow(x, -1.5));

  // warning: x contains both negative and positive values, y is non-integer
  x = double_interval(-3.5, 2.75);
  double c = pow(x, 1.5);
  double d = pow(x, -1.5);

  // warning: x contains zero, and y is negative (but integer)
  double e = pow(x, -3.); // odd y -> Top
  double f = pow(x, -2.); // even y -> [0.epsilon,+oo]

  // warning: positive/negative overflow (large values)
  x = double_interval(1LL<<50, 1LL<<62);
  double g = pow(x, 19.5);
  x = double_interval(-(1LL<<49), -.75);
  double h = pow(x, 23.0);

  // partial underflow
  x = double_interval(0.125, 0.25);
  double i = pow(x, 500.);
  Frama_C_show_each_i(i);
  x = double_interval(1.25, 2.0);
  double j = pow(x, -1024.);
  Frama_C_show_each_j(j);

  // "clean" cases
  // x contains zero
  double k = pow(x, 2.5);
  double l = pow(x, 3.5);
  // x does not contain zero
  x = double_interval(0.25, 4.125);
  double m = pow(x, -2.25);
  double n = pow(x, -1.5);
  double o = pow(x, 0.25);
  double p = pow(x, 1.625);
  double q = pow(x, 4.);
  // x contains only negative values
  x = double_interval(-3.5, -0.125);
  double r = pow(x, -2.);
  double s = pow(x, -1.);
}

void test_pow() {
  double x, y;
  // special cases: y = 0, x = 1
  y = double_interval(-18.5, 12.125);
  double a = pow(1.0, y);
  x = double_interval(-15.25, 32.75);
  double b = pow(x, 0.0);
  y = double_interval(-0.0, 0.0);
  double c = pow(x, y);

  // "general" case: y is not a singleton
  // test combinations of: x contains or not negative values;
  // y is assumed integer or not;
  // y crosses 1
  x = double_interval(-4.0,7.5);
  y = double_interval(2.125, 2.875);
  double d = pow(x, y);
  y = double_interval(2.125, 4.875);
  double e = pow(x, y);
  y = double_interval(2.0, 3.0);
  double f = pow(x, y);
  y = double_interval(-0.75, -0.);
  x = double_interval(0.125,7.5);
  double g = pow(x, y);

  // y crosses 1
  y = double_interval(0.25, 2.5);
  x = double_interval(0.5, 1.25);
  double h = pow(x, y);

  // x contains negative values
  x = double_interval(-1.0, 1.0);
  y = double_interval(-1.0, 1.0); // int(y) = [-1..1]
  double i = pow(x, y);
  x = double_interval(-1.0, 1.0);
  y = double_interval(-0.5, 1.0); // int(y) = [0..1]
  double j = pow(x, y);

  // y is positive but spans both even and odd numbers, and
  // x crosses 0; minimum value is (-13)^5
  x = double_interval(-13.,430.);
  y = double_interval(3.,6.);
  double k = pow(x,y);

  // y is positive but spans both even and odd numbers, and x is negative
  x = double_interval(-4.,-2.); // maximum is (-4)^4
  y = double_interval(3.,5.);
  double l = pow(x,y);

  // positive interval containing -0.0
  x = double_interval(-0.,10.);
  y = double_interval(-0.,5.);
  double m = pow(x,y);

  // x contains -0.0, but y contains no odd integer
  x = double_interval(-0.0,10.0);
  y = double_interval(-3.5,4.5);
  double n = pow(x,y);

  // negative x and non-integer y
  x = double_interval(-5.,-0.5);
  y = double_interval(-0.875,-0.125);
  assert_bottom(pow(x,y));
  y = double_interval(-0.25,0.25);
  double o = pow(x,y);
  y = double_interval(10.,11.5);
  double p = pow(x,y);
  y = double_interval(-9876.5,1234.5);
  double q = pow(x,y);

  // +oo for all values of x and y
  x = double_interval(1e10, 1e11);
  y = double_interval(1e14, 1e15);
  assert_bottom(pow(x,y));

  // -oo for all values of x and y
  x = double_interval(-1e10, -1e11);
  y = double_interval(1e14, 1e15);
  assert_bottom(pow(x,y));

  y = double_interval(-0.5, 9.5);
  double r = pow(10.0,y);

}

void test_powf_singleton_exp() {
  float f32__x;
  f32__x = double_interval(-3.5, -2.75);
  // error: f32__x contains only negative values, and f32__y is a non-integer
  assert_bottom(powf(f32__x, 1.5));
  // again, but with negative f32__y
  assert_bottom(powf(f32__x, -1.5));

  // warning: f32__x contains both negative and positive values, f32__y is non-integer
  f32__x = double_interval(-3.5, 2.75);
  float f32__c = powf(f32__x, 1.5);
  float f32__d = powf(f32__x, -1.5);

  // warning: f32__x contains zero, and f32__y is negative (but integer)
  float f32__e = powf(f32__x, -3.); // odd f32__y -> Top
  float f32__f = powf(f32__x, -2.); // even f32__y -> [0,+oo]

  // warning: positive/negative overflow (large values)
  f32__x = double_interval(1LL<<20, 1LL<<21);
  float f32__g = powf(f32__x, 5.8);
  f32__x = double_interval(-(1LL<<49), -.75);
  float f32__h = powf(f32__x, 23.0);

  // partial underflow
  f32__x = double_interval(0.125, 0.25);
  float f32__i = powf(f32__x, 60.);
  Frama_C_show_each_i(f32__i);
  f32__x = double_interval(1.25, 2.0);
  float f32__j = powf(f32__x, -1024.);
  Frama_C_show_each_j(f32__j);

  // "clean" cases
  // f32__x contains zero
  float f32__k = powf(f32__x, 2.5);
  float f32__l = powf(f32__x, 3.5);
  // f32__x does not contain zero
  f32__x = double_interval(0.25, 4.125);
  float f32__m = powf(f32__x, -2.25);
  float f32__n = powf(f32__x, -1.5);
  float f32__o = powf(f32__x, 0.25);
  float f32__p = powf(f32__x, 1.625);
  float f32__q = powf(f32__x, 4.);
  // f32__x contains only negative values
  f32__x = double_interval(-3.5, -0.125);
  float f32__r = powf(f32__x, -2.);
  float f32__s = powf(f32__x, -1.);
}

void test_powf() {
  float f32__x, f32__y;
  // special cases: f32__y = 0, f32__x = 1
  f32__y = double_interval(-18.5, 12.125);
  float f32__a = powf(1.0, f32__y);
  f32__x = double_interval(-15.25, 32.75);
  float f32__b = powf(f32__x, 0.0);
  f32__y = double_interval(-0.0, 0.0);
  float f32__c = powf(f32__x, f32__y);

  // "general" case: f32__y is not a singleton
  // test combinations of: f32__x contains or not negative values;
  // f32__y is assumed integer or not;
  // f32__y crosses 1
  f32__x = double_interval(-4.0,7.5);
  f32__y = double_interval(2.125, 2.875);
  float f32__d = powf(f32__x, f32__y);
  f32__y = double_interval(2.125, 4.875);
  float f32__e = powf(f32__x, f32__y);
  f32__y = double_interval(2.0, 3.0);
  float f32__f = powf(f32__x, f32__y);
  f32__y = double_interval(-0.75, -0.);
  f32__x = double_interval(0.125,7.5);
  float f32__g = powf(f32__x, f32__y);

  // f32__y crosses 1
  f32__y = double_interval(0.25, 2.5);
  f32__x = double_interval(0.5, 1.25);
  float f32__h = powf(f32__x, f32__y);

  // f32__x contains negative values, f32__y assumed integer
  f32__x = double_interval(-1.0, 1.0);
  f32__y = double_interval(-1.0, 1.0);
  float f32__i = powf(f32__x, f32__y);

  // f32__x contains negative values, but f32__y not assumed integer
  f32__x = double_interval(-1.0, 1.0);
  f32__y = double_interval(-0.5, 1.0);
  float f32__j = powf(f32__x, f32__y);

  // f32__y is positive but spans both even and odd numbers, and
  // f32__x crosses 0
  f32__x = double_interval(-13.,430.);
  f32__y = double_interval(3.,6.);
  float f32__k = powf(f32__x,f32__y);

  // f32__y is positive but spans both even and odd numbers, and x is negative
  f32__x = double_interval(-4.,-2.);
  f32__y = double_interval(3.,5.);
  float f32__l = powf(f32__x,f32__y);

  // positive interval containing -0.0
  f32__x = double_interval(-0.,10.);
  f32__y = double_interval(-0.,5.);
  float f32__m = powf(f32__x,f32__y);
}

void test_fmod_det() {
  double a = fmod(4.0, 2.25);
  double b = fmod(-4.0, 2.25);
  double c = fmod(-0.0, 2.25);
  double d = fmod(0.0, 2.25);
  double e = fmod(0.0, 0.0009765625);
  double f = fmod(1.25, 0.0009765625);
  assert_bottom(fmod(0.125, 0.0));
}

void test_fmod() {
  double x, y;
  x = double_interval(2., 9.5);
  double a = fmod(x, 1.5);
  double b = fmod(x, -1.5);
  double c = fmod(-x, 1.5);
  double d = fmod(-x, -1.5);
  x = double_interval(.75, 1.25);
  double e = fmod(x, 1.5);
  double f = fmod(x, -1.5);
  x = double_interval(4., 5.5);
  y = double_interval(6.5, 8.);
  double g = fmod(x, y);
  x = double_interval(6.5, 8.);
  y = double_interval(4., 5.5);
  double h = fmod(x, y);
  double i = fmod(x, -y);
  x = double_interval(-9., 8.75);
  y = double_interval(-2., 4.5);
  double j = fmod(x, y);
  x = double_interval(-2., 4.5);
  y = double_interval(-9., 8.75);
  double k = fmod(x, y);
  y = double_interval(-0.0, 0.0);
  assert_bottom(fmod(x, y));
  y = double_interval(0.0, 0.125);
  double l = fmod(x, y);
  y = double_interval(-0.125, -0.0);
  double m = fmod(x, y);
  x = double_interval(1e308, 1.1e308);
  double n = fmod(x, 0.001953125); // imprecise
  x = double_interval(-1.1e308, -1e308);
  double o = fmod(x, 0.001953125); // imprecise
  x = double_interval(1e308, 1.00001e308);
  double p = fmod(x, 0.001953125); // imprecise
  x = double_interval(1.600000000000001e11, 1.600000000000005e11);
  double q = fmod(x, 0.001953125); // precise
  x = double_interval(1.759218604000011e13, 1759218604000015e13);
  double r = fmod(x, 0.00195313); // imprecise
  x = double_interval(0.5, 0.505);
  double s = fmod(x, 0.1);
  x = -double_interval(0.5, 0.505);
  double t = fmod(x, 0.1);
  x = double_interval(-8, -6.5);
  double u = fmod(x, 4);
  x = double_interval(6, 7);
  y = double_interval(-5,-4);
  double v1 = fmod(x, y);
  y = double_interval(4,5);
  double v2 = fmod(x, y);
  x = double_interval(-7, -6);
  double v3 = fmod(x, y);
  y = double_interval(-5,-4);
  double v4 = fmod(x, y);
  x = double_interval(2,3);
  y = double_interval(-4,5);
  double w1 = fmod(x, y);
  x = double_interval(-2,3);
  double w2 = fmod(x, y);
  x = double_interval(-3,2);
  double w3 = fmod(x, y);
}

void test_sqrt_det() {
  double a = sqrt(42.125);
  double b = sqrt(0.125);
  assert_bottom(sqrt(-2.));
  double c = sqrt(0.);
  double d = sqrt(-0.);
}

void test_sqrt() {
  double x = double_interval(-0.0, 5.0);
  double a = sqrt(x);
  x = double_interval(-6.5, 0.0);
  double b = sqrt(x);
  x = double_interval(-6.5, -0.0);
  double c = sqrt(x);
  x = double_interval(-6.5, -0.1);
  assert_bottom(sqrt(x));
}

void test_sqrtf_det() {
  float f32__a = sqrtf(42.125);
  float f32__b = sqrtf(0.125);
  assert_bottom(sqrtf(-2.));
  float f32__c = sqrtf(0.);
  float f32__d = sqrtf(-0.);
}

void test_sqrtf() {
  float f32__x = double_interval(-0.0, 5.0);
  float f32__a = sqrtf(f32__x);
  f32__x = double_interval(-6.5, 0.0);
  float b = sqrtf(f32__x);
  f32__x = double_interval(-6.5, -0.0);
  float c = sqrtf(f32__x);
  f32__x = double_interval(-6.5, -0.1);
  assert_bottom(sqrt(f32__x));
}

void test_exp_det() {
  double a = exp(42.125);
  double b = exp(-2.);
  double c = exp(0.);
  double d = exp(-0.);
  double e = exp(-1.);
  assert_bottom(exp(1e100));
}

void test_expf_det() {
  float f32__a = expf(42.125);
  float f32__b = expf(-2.);
  float f32__c = expf(0.);
  float f32__d = expf(-0.);
  float f32__e = expf(-1.);
}

void test_log_det() {
  double a = log(42.125);
  double b = log(0.125);
  assert_bottom(log(-2.));
  assert_bottom(log(0.));
  assert_bottom(log(-0.));
  assert_bottom(log(-1.));
}

void test_logf_det() {
  float f32__a = logf(42.125);
  float f32__b = logf(0.125);
  assert_bottom(logf(-2.));
  assert_bottom(logf(0.));
  assert_bottom(logf(-0.));
  assert_bottom(logf(-1.));
}

void test_log10_det() {
  double a = log10(42.125);
  double b = log10(0.125);
  assert_bottom(log10(-2.));
  assert_bottom(log10(0.));
  assert_bottom(log10(-0.));
  assert_bottom(log10(-1.));
}

void test_log10f_det() {
  float f32__a = log10f(42.125);
  float f32__b = log10f(0.125);
  assert_bottom(log10f(-2.));
  assert_bottom(log10f(0.));
  assert_bottom(log10f(-0.));
  assert_bottom(log10f(-1.));
}

void test_diff_pow_powf() {
  double d = pow(0.11, 30.);
  float f32__f = powf(0.11, 30.);
}

void test_floor_det() {
  double a = floor(1.5);
  double b = floor(0.5);
  double c = floor(0.0);
  double d = floor(-0.0);
  double e = floor(-0.5);
  double f = floor(-1.5);
}

void test_ceil_det() {
  double a = ceil(1.5);
  double b = ceil(0.5);
  double c = ceil(0.0);
  double d = ceil(-0.0);
  double e = ceil(-0.5);
  double f = ceil(-1.5);
}

void test_trunc_det() {
  double a = trunc(1.5);
  double b = trunc(0.5);
  double c = trunc(0.0);
  double d = trunc(-0.0);
  double e = trunc(-0.5);
  double f = trunc(-1.5);
}

void test_round_det() {
  double a = round(1.5);
  double b = round(0.5);
  double c = round(0.0);
  double d = round(-0.0);
  double e = round(-0.5);
  double f = round(-1.5);
}

void test_floor() {
  double x;
  x = double_interval(-0.5,1.5);
  double a = floor(x);
  x = double_interval(-0.0,0.5);
  double b = floor(x);
  x = double_interval(-2.5,-0.5);
  double c = floor(x);
}

void test_ceil() {
  double x;
  x = double_interval(-0.5,1.5);
  double a = ceil(x);
  x = double_interval(-0.0,0.5);
  double b = ceil(x);
  x = double_interval(-2.5,-0.5);
  double c = ceil(x);
}

void test_trunc() {
  double x;
  x = double_interval(-0.5,1.5);
  double a = trunc(x);
  x = double_interval(-0.0,0.5);
  double b = trunc(x);
  x = double_interval(-2.5,-0.5);
  double c = trunc(x);
}

void test_round() {
  double x;
  x = double_interval(-0.5,1.5);
  double a = round(x);
  x = double_interval(-0.0,0.5);
  double b = round(x);
  x = double_interval(-2.5,-0.5);
  double c = round(x);
}

void test_floorf_det() {
  float f32__a = floorf(1.5);
  float f32__b = floorf(0.5);
  float f32__c = floorf(0.0);
  float f32__d = floorf(-0.0);
  float f32__e = floorf(-0.5);
  float f32__f = floorf(-1.5);
}

void test_ceilf_det() {
  float f32__a = ceilf(1.5);
  float f32__b = ceilf(0.5);
  float f32__c = ceilf(0.0);
  float f32__d = ceilf(-0.0);
  float f32__e = ceilf(-0.5);
  float f32__f = ceilf(-1.5);
}

void test_truncf_det() {
  float f32__a = truncf(1.5);
  float f32__b = truncf(0.5);
  float f32__c = truncf(0.0);
  float f32__d = truncf(-0.0);
  float f32__e = truncf(-0.5);
  float f32__f = truncf(-1.5);
}

void test_roundf_det() {
  float f32__a = roundf(1.5);
  float f32__b = roundf(0.5);
  float f32__c = roundf(0.0);
  float f32__d = roundf(-0.0);
  float f32__e = roundf(-0.5);
  float f32__f = roundf(-1.5);
}

void test_floorf() {
  float f32__x;
  f32__x = double_interval(-0.5,1.5);
  float f32__a = floorf(f32__x);
  f32__x = double_interval(-0.0,0.5);
  float f32__b = floorf(f32__x);
  f32__x = double_interval(-2.5,-0.5);
  float f32__c = floorf(f32__x);
}

void test_ceilf() {
  float f32__x;
  f32__x = double_interval(-0.5,1.5);
  float f32__a = ceilf(f32__x);
  f32__x = double_interval(-0.0,0.5);
  float f32__b = ceilf(f32__x);
  f32__x = double_interval(-2.5,-0.5);
  float f32__c = ceilf(f32__x);
}

void test_truncf() {
  float f32__x;
  f32__x = double_interval(-0.5,1.5);
  float f32__a = truncf(f32__x);
  f32__x = double_interval(-0.0,0.5);
  float f32__b = truncf(f32__x);
  f32__x = double_interval(-2.5,-0.5);
  float f32__c = truncf(f32__x);
}

void test_roundf() {
  float f32__x;
  f32__x = double_interval(-0.5,1.5);
  float f32__a = roundf(f32__x);
  f32__x = double_interval(-0.0,0.5);
  float f32__b = roundf(f32__x);
  f32__x = double_interval(-2.5,-0.5);
  float f32__c = roundf(f32__x);
}

int main() {
  test_cos_det();
  test_sin_det();
  test_atan2_det();
  test_atan2();
  test_pow_det();
  test_pow_singleton_exp();
  test_pow();
  test_fmod_det();
  test_fmod();
  test_sqrt_det();
  test_sqrt();
  test_exp_det();
  test_log_det();
  test_log10_det();

  test_powf_det();
  test_powf_singleton_exp();
  test_powf();
  test_sqrtf_det();
  test_sqrtf();
  test_expf_det();
  test_logf_det();
  test_log10f_det();

  test_diff_pow_powf();

  test_floor_det();
  test_ceil_det();
  test_trunc_det();
  test_round_det();
  test_floor();
  test_ceil();
  test_trunc();
  test_round();

  test_floorf_det();
  test_ceilf_det();
  test_truncf_det();
  test_roundf_det();
  test_floorf();
  test_ceilf();
  test_truncf();
  test_roundf();

  return 0;
}
