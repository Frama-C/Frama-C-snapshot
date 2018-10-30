/* run.config
   OPT: -eva -eva-numerors-domain -eva-msg-key=d-numerors
*/

/* Tests for the numerors domain, that computes absolute and relative errors
   of floating-point computations. */

#include <__fc_builtin.h>
#include <math.h>

#define TRUE 1
#define FALSE 0

double ex0(double x) {
  int n = 0 ;
  x = x * x ;
  //@ loop unroll 14 ;
  while (x >= 1) {
    x /= 2.0 ;
    n += 1 ;
  }
  double y = (1.0 - x);
  double lx = (y * (1.0 + (y * ((1.0 / 2.0) + (y * ((1.0 / 3.0) + (y * ((1.0 / 4.0) + (y * (1.0 / 5.0))))))))));
  return (lx * (n / 0.69314718056));
}

double ex1(double i) {
  double x = i * i ;
  double temp ;
  if (x < 2.0)
    temp = x ;
  else
    temp = 2.0 ;
  return temp ;
}

double ex2(double cst, double i) {
  double x = (i * i);
  double temp;
  if ((x < cst)) {
    temp = ((7.0 * x) - 5.0);
  } else {
    temp = cst;
  }
  return temp;
}

double ex3(double x) {
  return (sqrt(2.0 * x + 3.0)) / (2.0 * sqrt(x) + 3.0) ;
}

double ex3b(double re, double im) {
  return 0.5 * sqrt(2.0 * sqrt(re * re + im * im) + re) ;
}

double ex6(double x3, double x4, double x5, double x2, double x6, double x1) {
  return (((((x2 * x5) + (x3 * x6)) - (x2 * x3)) - (x5 * x6)) + (x1 * (((((-x1 + x2) + x3) - x4) + x5) + x6)));
}

double ex7(double t) {
  return (t / (t + 1.0));
}

double ex8(double x, double y) {
  double t = (x * y);
  return ((t - 1.0) / ((t * t) - 1.0));
}

double ex9(double x0, double x2, double x1) {
  double p0 = ((x0 + x1) - x2);
  double p1 = ((x1 + x2) - x0);
  double p2 = ((x2 + x0) - x1);
  return ((p0 + p1) + p2);
}

double ex10(double x3, double x4, double x0, double x5, double x2, double x7, double x6, double x1) {
  return (((((((x0 + x1) + x2) + x3) + x4) + x5) + x6) + x7);
}

double ex11(double x, double y) {
  return ((x + y) / (x - y));
}

double ex12(double w1, double a1, double a0, double w0, double w2, double a2, double m0, double m2, double m1) {
  double v2 = ((w2 * (0.0 - m2)) * (-3.0 * ((1.0 * (a2 / w2)) * (a2 / w2))));
  double v1 = ((w1 * (0.0 - m1)) * (-3.0 * ((1.0 * (a1 / w1)) * (a1 / w1))));
  double v0 = ((w0 * (0.0 - m0)) * (-3.0 * ((1.0 * (a0 / w0)) * (a0 / w0))));
  return (0.0 + ((v0 * 1.0) + ((v1 * 1.0) + ((v2 * 1.0) + 0.0))));
}

double ex13(double x) {
  double r1 = (x - 1.0);
  double r2 = (x * x);
  return (r1 / (r2 - 1.0));
}

double ex14(double x) {
  return (1.0 / (x + 1.0));
}

double ex15(double v, double u, double T) {
  double t1 = (331.4 + (0.6 * T));
  return ((-t1 * v) / ((t1 + u) * (t1 + u)));
}

double ex16(double v, double u, double T) {
  double t1 = (331.4 + (0.6 * T));
  return ((-t1 * v) / ((t1 + u) * (t1 + u)));
}

double ex17(double v, double u, double T) {
  double t1 = (331.4 + (0.6 * T));
  return ((-t1 * v) / ((t1 + u) * (t1 + u)));
}

double ex18(double x3, double x2, double x1) {
  return (((-(x1 * x2) - ((2.0 * x2) * x3)) - x1) - x3);
}

double ex19(double x3, double x2, double x1) {
  return (((((((2.0 * x1) * x2) * x3) + ((3.0 * x3) * x3)) - (((x2 * x1) * x2) * x3)) + ((3.0 * x3) * x3)) - x2);
}

double ex20(double v, double w, double r) {
  return (((3.0 + (2.0 / (r * r))) - (((0.125 * (3.0 - (2.0 * v))) * (((w * w) * r) * r)) / (1.0 - v))) - 4.5);
}

double ex21(double v, double w, double r) {
  return (((6.0 * v) - (((0.5 * v) * (((w * w) * r) * r)) / (1.0 - v))) - 2.5);
}

double ex22(double v, double w, double r) {
  return (((3.0 - (2.0 / (r * r))) - (((0.125 * (1.0 + (2.0 * v))) * (((w * w) * r) * r)) / (1.0 - v))) - 0.5);
}

double ex23(double x) {
  double r = 4.0;
  double K = 1.11;
  return ((r * x) / (1.0 + (x / K)));
}

double ex24(double x) {
  double r = 4.0;
  double K = 1.11;
  return (((r * x) * x) / (1.0 + ((x / K) * (x / K))));
}

double ex25(double v) {
  double p = 35000000.0;
  double a = 0.401;
  double b = 4.27e-05;
  double t = 300.0;
  double n = 1000.0;
  double k = 1.3806503e-23;
  return (((p + ((a * (n / v)) * (n / v))) * (v - (n * b))) - ((k * n) * t));
}

double ex26(double x) {
  return (((x - (((x * x) * x) / 6.0)) + (((((x * x) * x) * x) * x) / 120.0)) - (((((((x * x) * x) * x) * x) * x) * x) / 5040.0));
}

double ex27(double x) {
  return ((((1.0 + (0.5 * x)) - ((0.125 * x) * x)) + (((0.0625 * x) * x) * x)) - ((((0.0390625 * x) * x) * x) * x));
}

double ex28(double x) {
  return ((0.954929658551372 * x) - (0.12900613773279798 * ((x * x) * x)));
}

int main() {
  //log_approx
  {
    double x = Frama_C_double_interval(1.0, 100.0) ;
    double res = ex0(x) ;
    Frama_C_domain_show_each_ex0(res) ;
  }
  //conditional_ex
  {
    double i = Frama_C_double_interval(1.0, 100.0) ;
    double res = ex1(i) ;
    Frama_C_domain_show_each_ex1(res) ;
  }
  //conditional_1
  {
    double cst = 500.0 ;
    double i = Frama_C_double_interval(1.0, 100.0) ;
    double res = ex2(cst, i) ;
    Frama_C_domain_show_each_ex2(res) ;
  }
  //sqrt_1
  {
    double x = Frama_C_double_interval(1.0, 1000.0) ;
    double res = ex3(x) ;
    Frama_C_domain_show_each_ex3(res) ;
  }
  //complex_sqrt
  {
    double re = Frama_C_double_interval(1.0, 10.0) ;
    double im = Frama_C_double_interval(1.0, 10.0) ;
    double res = ex3b(re, im) ;
    Frama_C_domain_show_each_ex3b(res) ;
  }
  //kepler0
  {
    double x1 = Frama_C_double_interval(4.0, 6.36) ;
    double x2 = Frama_C_double_interval(0.0001, 0.00011) ;
    double x3 = Frama_C_double_interval(40.0, 63.6) ;
    double x4 = Frama_C_double_interval(-6.36, -4.0) ;
    double x5 = Frama_C_double_interval(4.0, 6.36) ;
    double x6 = Frama_C_double_interval(4.0, 6.36) ;
    double res = ex6(x3, x4, x5, x2, x6, x1) ;
    Frama_C_domain_show_each_ex6(res) ;
  }
  //intro-example
  {
    double t = Frama_C_double_interval(0.0, 999.0) ;
    double res = ex7(t) ;
    Frama_C_domain_show_each_ex7(res) ;
  }
  //sec4-example
  {
    double x = Frama_C_double_interval(1.001, 2.0) ;
    double y = Frama_C_double_interval(1.001, 2.0) ;
    double res = ex8(x, y) ;
    Frama_C_domain_show_each_ex8(res) ;
  }
  //test01_sum3
  {
    double x0 = Frama_C_double_interval(1.0, 2.0) ;
    double x1 = Frama_C_double_interval(1.0, 2.0) ;
    double x2 = Frama_C_double_interval(1.0, 2.0) ;
    double res = ex9(x0, x2, x1) ;
    Frama_C_domain_show_each_ex9(res) ;
  }
  //test02_sum8
  {
    double x0 = Frama_C_double_interval(1.0, 2.0) ;
    double x1 = Frama_C_double_interval(1.0, 2.0) ;
    double x2 = Frama_C_double_interval(1.0, 2.0) ;
    double x3 = Frama_C_double_interval(1.0, 2.0) ;
    double x4 = Frama_C_double_interval(1.0, 2.0) ;
    double x5 = Frama_C_double_interval(1.0, 2.0) ;
    double x6 = Frama_C_double_interval(1.0, 2.0) ;
    double x7 = Frama_C_double_interval(1.0, 2.0) ;
    double res = ex10(x3, x4, x0, x5, x2, x7, x6, x1) ;
    DPRINTFrama_C_domain_show_each_ex10(res) ;
  }
  //test03_nonlin2
  {
    double x = Frama_C_double_interval(0.0, 1.0) ;
    double y = Frama_C_double_interval(-1.0, -0.1) ;
    double res = ex11(x, y) ;
    Frama_C_domain_show_each_ex11(res) ;
  }
  //test04_dqmom9
  {
    double m0 = Frama_C_double_interval(-1.0, 1.0) ;
    double m1 = Frama_C_double_interval(-1.0, 1.0) ;
    double m2 = Frama_C_double_interval(-1.0, 1.0) ;
    double w0 = Frama_C_double_interval(1e-05, 1.0) ;
    double w1 = Frama_C_double_interval(1e-05, 1.0) ;
    double w2 = Frama_C_double_interval(1e-05, 1.0) ;
    double a0 = Frama_C_double_interval(1e-05, 1.0) ;
    double a1 = Frama_C_double_interval(1e-05, 1.0) ;
    double a2 = Frama_C_double_interval(1e-05, 1.0) ;
    double res = ex12(w1, a1, a0, w0, w2, a2, m0, m2, m1) ;
    Frama_C_domain_show_each_ex12(res) ;
  }
  //test05_nonlin1_r4
  {
    double x = Frama_C_double_interval(1.00001, 2.0) ;
    double res = ex13(x) ;
    Frama_C_domain_show_each_ex13(res) ;
  }
  //test05_nonlin1_test2
  {
    double x = Frama_C_double_interval(1.00001, 2.0) ;
    double res = ex14(x) ;
    Frama_C_domain_show_each_ex14(res) ;
  }
  //doppler1
  {
    double u = Frama_C_double_interval(-100.0, 100.0) ;
    double v = Frama_C_double_interval(20.0, 20000.0) ;
    double T = Frama_C_double_interval(-30.0, 50.0) ;
    double res = ex15(v, u, T) ;
    Frama_C_domain_show_each_ex15(res) ;
  }
  //doppler2
  {
    double u = Frama_C_double_interval(-125.0, 125.0) ;
    double v = Frama_C_double_interval(15.0, 25000.0) ;
    double T = Frama_C_double_interval(-40.0, 60.0) ;
    double res = ex16(v, u, T) ;
    Frama_C_domain_show_each_ex16(res) ;
  }
  //doppler3
  {
    double u = Frama_C_double_interval(-30.0, 120.0) ;
    double v = Frama_C_double_interval(320.0, 20300.0) ;
    double T = Frama_C_double_interval(-50.0, 30.0) ;
    double res = ex17(v, u, T) ;
    Frama_C_domain_show_each_ex17(res) ;
  }
  //rigidBody1
  {
    double x1 = Frama_C_double_interval(-15.0, -0.1) ;
    double x2 = Frama_C_double_interval(0.1, 15.0) ;
    double x3 = Frama_C_double_interval(-15.0, -0.1) ;
    double res = ex18(x3, x2, x1) ;
    Frama_C_domain_show_each_ex18(res) ;
  }
  //rigidBody2
  {
    double x1 = Frama_C_double_interval(-15.0, -11.25) ;
    double x2 = Frama_C_double_interval(-15.0, -11.25) ;
    double x3 = Frama_C_double_interval(-15.0, -11.25) ;
    double res = ex19(x3, x2, x1) ;
    Frama_C_domain_show_each_ex19(res) ;
  }
  //turbine1
  {
    double v = Frama_C_double_interval(-4.5, -0.3) ;
    double w = Frama_C_double_interval(0.4, 0.9) ;
    double r = Frama_C_double_interval(3.8, 7.8) ;
    double res = ex20(v, w, r) ;
    Frama_C_domain_show_each_ex20(res) ;
  }
  //turbine2
  {
    double v = Frama_C_double_interval(-4.5, -3.3) ;
    double w = Frama_C_double_interval(-0.4, -0.1) ;
    double r = Frama_C_double_interval(3.8, 7.8) ;
    double res = ex21(v, w, r) ;
    Frama_C_domain_show_each_ex21(res) ;
  }
  //turbine3
  {
    double v = Frama_C_double_interval(-4.5, -0.3) ;
    double w = Frama_C_double_interval(0.4, 0.9) ;
    double r = Frama_C_double_interval(3.8, 7.8) ;
    double res = ex22(v, w, r) ;
    Frama_C_domain_show_each_ex22(res) ;
  }
  //verhulst
  {
    double x = Frama_C_double_interval(0.1, 0.3) ;
    double res = ex23(x) ;
    Frama_C_domain_show_each_ex23(res) ;
  }
  //predatorPrey
  {
    double x = Frama_C_double_interval(0.1, 0.3) ;
    double res = ex24(x) ;
    Frama_C_domain_show_each_ex24(res) ;
  }
  //carbonGas
  {
    double v = Frama_C_double_interval(0.1, 0.5) ;
    double res = ex25(v) ;
    Frama_C_domain_show_each_ex25(res) ;
  }
  //sine
  {
    double x = Frama_C_double_interval(0.875, 1.57079632679) ;
    double res = ex26(x) ;
    Frama_C_domain_show_each_ex26(res) ;
  }
  //sqroot
  {
    double x = Frama_C_double_interval(0.0, 1.0) ;
    double res = ex27(x) ;
    Frama_C_domain_show_each_ex27(res) ;
  }
  //sineOrder3
  {
    double x = Frama_C_double_interval(-2.0, -1.125) ;
    double res = ex28(x) ;
    Frama_C_domain_show_each_ex28(res) ;
  }
  return 0 ;
}
