/* run.config
   STDOPT: +"-warn-decimal-float all" +"-float-hex"
*/

volatile v;

void test_is_finite(void);

int main() {
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

  test_is_finite();
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
  /*@ assert \is_finite((float)(0.1/0.0)); */

  extern int undet;
  extern int top_int;

  /* Reduces f1 from top_ival to top_float. */
  float f1;
  * (int *) (&f1) = top_int;
  /*@ assert \is_finite(f1); */

  /* Should not reduce the fs. */
  float f2, f3;
  * (int *) (&f2) = top_int;
  * (int *) (&f3) = top_int;
  float *p;
  if (undet) p = &f2; else p = &f3;
  /*@ assert \is_finite(*p); */

  /* Returns the exact value from the spec. */
  float g1 = my_fabs(-3.3);
  float g2 = my_fabs(3.3);

  /* Tests that we could approximate the result */
  float g3 = my_ratio(-3.3);

  /* Tests that is_finite validates the input. */
  float g4 = my_ratio_body(-3.3);
}
