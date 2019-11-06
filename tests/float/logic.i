/* run.config*
   STDOPT: #"-warn-decimal-float all -float-hex"
   STDOPT: #"-warn-decimal-float all -float-hex -warn-special-float none"
*/

volatile v;
volatile float any_float;
volatile double any_double;

void test_equality () {
  if (v) {
    double d = 0.1;
    //@ assert !(d == 0.1);
  }

  if (v) {
    double d = 0.1;
    //@ assert d == 0.1f;
  }

  if (v) {
    float f = 0.1;
    //@ assert !(f == 0.1);
  }

  // assert 0.1 == v;
}

/* Tests the evaluation of logic comparison operators. */
void test_comparison_evaluation () {
  /* Evaluation with singletons. */
  float zero = 0.;
  float minus_zero = -0.;
  float one = 1.;
  /*@ check \eq_float(one, one); */                 // true
  /*@ check \eq_float(zero, minus_zero); */         // true
  /*@ check \ne_float(zero, one); */                // true
  /*@ check \lt_float(zero, one); */                // true
  /*@ check \ge_float(minus_zero, zero); */         // true
  /*@ check \eq_float(zero, one); */                // false
  /*@ check \ne_float(one, one); */                 // false
  /*@ check \ne_float(zero, minus_zero); */         // false
  /*@ check \lt_float(zero, minus_zero); */         // false
  if (v) {
    float inf = 1. / 0.;
    float nan = 0. / 0.;
    /*@ check \gt_float(inf, one); */              // true
    /*@ check \ge_float(inf, inf); */              // true
    /*@ check \le_float(inf, one); */              // false
    /*@ check \lt_float(inf, inf); */              // false
    /*@ check \ne_float(zero, nan); */             // true
    /*@ check \ne_float(inf, nan); */              // true
    /*@ check \ne_float(nan, nan); */              // true
    /*@ check \eq_float(one, nan); */              // false
    /*@ check \eq_float(nan, nan); */              // false
    /*@ check \ge_float(one, nan); */              // false
    /*@ check \ge_float(inf, nan); */              // false
    /*@ check \ge_float(nan, nan); */              // false
  }
  /* Evaluation with intervals. */
  float higher = v ? 3.14 : 12.5;
  float middle = v ? 3.14 : -3.14;
  float lower = v ? -3.14 : -11.1;
  /*@ check \eq_float(middle, middle); */           // unknown
  /*@ check \ne_float(middle, middle); */           // unknown
  /*@ check \gt_float(middle, middle); */           // unknown
  /*@ check \ne_float(higher, lower); */            // true
  /*@ check \eq_float(higher, lower); */            // false
  /*@ check \ge_float(higher, middle); */           // true
  /*@ check \ge_float(higher, lower); */            // true
  /*@ check \gt_float(higher, middle); */           // unknown
  /*@ check \gt_float(higher, lower); */            // true
  /*@ check \ge_float(middle, higher); */           // unknown
  /*@ check \gt_float(middle, higher); */           // false
  /*@ check \gt_float(lower, higher); */            // false
}

/* Tests the reduction of a variable [d] evaluating to any double by the
   evaluation of a logic comparison with [bound]. */
void test_comparison_reduction (double bound) {
  double d = any_double;
  if (v) {
    /*@ assert d == bound; */
    Frama_C_show_each_eq(bound, d);
  }
  if (v) {
    /*@ assert d < bound; */
    Frama_C_show_each_lt(bound, d);
  }
  if (v) {
    /*@ assert d <= bound; */
    Frama_C_show_each_lt(bound, d);
  }
  if (v) {
    /*@ assert \eq_double(d, bound); */
    Frama_C_show_each_eq_double(bound, d);
  }
  if (v) {
    /*@ assert \lt_double(d, bound); */
    Frama_C_show_each_lt_double(bound, d);
  }
  if (v) {
    /*@ assert \le_double(d, bound); */
    Frama_C_show_each_le_double(bound, d);
  }
  if (v) {
    /*@ assert \ne_double(d, bound); */
    Frama_C_show_each_ne_double(bound, d);
  }
}

/* Tests the evaluation and reduction by the builtin comparison operators
   eq_float, lt_float, etc. */
void test_builtin_comparisons () {
  test_comparison_evaluation();
  /* Comparisons with a singleton bound. */
  test_comparison_reduction(-1.);
  test_comparison_reduction(-0.);
  test_comparison_reduction(0.);
  test_comparison_reduction(0.1);
  /* Comparisons with an interval bound. */
  double bound = -10.;
  if (v) bound = -1.;
  test_comparison_reduction(bound);
  if (v) bound = -0.;
  test_comparison_reduction(bound);
  if (v) bound = 0.;
  test_comparison_reduction(bound);
  if (v) bound = 0.1;
  test_comparison_reduction(bound);
  /* Comparisons with an infinite or NaN bound. */
  if (v) {
    test_comparison_reduction(1. / 0.);
    test_comparison_reduction(0. / 0.);
  }
}


/*@ assigns \result \from f;
    ensures \is_finite(\result);
    ensures \result >= 0;
    ensures \result == f || \result == -f; */
float my_fabs(float f);

/*@ assigns \nothing;
    ensures \is_finite(\result);
    ensures -1. < \result < 1.; */
float my_ratio_body(float f){ return f/(my_fabs( f) + 0.5); }

/*@ assigns \nothing;
    ensures \is_finite(\result);
    ensures -1. < \result < 1.; */
float my_ratio(float f);

void test_is_finite(void) {

  /*@ assert \is_finite((float)0.1); */
  /* assert \is_finite((float)(0.1/0.0)); */ // false because the computation is done in real (result undefined)

  extern int undet;
  extern volatile int top_int;

  /* Reduces f1 from top_ival to top_float. */
  float f1;
  * (int *) (&f1) = top_int;
  /*@ assert \is_finite(f1); */ // false, but we want to test the reduction

  /* Should not reduce the fs. */
  float f2, f3;
  * (int *) (&f2) = top_int;
  * (int *) (&f3) = top_int;
  float *p;
  if (undet) p = &f2; else p = &f3;
  /*@ assert \is_finite(*p); */ // false, but we cannot learn anything by reduction (in the Cvalue domain

  /* Returns the exact value from the spec. */
  float g1 = my_fabs(-3.3);
  float g2 = my_fabs(3.3);

  /* Tests that we could approximate the result */
  float g3 = my_ratio(-3.3);

  /* Tests that is_finite validates the input. */
  float g4 = my_ratio_body(-3.3);
}


int main () {
  test_equality();
  test_builtin_comparisons();
  test_is_finite();
}
