/* run.config* */

volatile int rand;

//@ requires f1 >= 0. && f2 <= 0.;
void main1(float f1, float f2) {
  float f = 0.;

  if (f == -0.) {
    Frama_C_show_each(1);
  } else {
    Frama_C_show_each(2);
  }

  f = -0.;
  if (f == 0.) {
    Frama_C_show_each(1);
  } else {
    Frama_C_show_each(2);
  }

  if (f1 != 0) {
    Frama_C_show_each_1(f1);
  } else {
    Frama_C_show_each_2(f1);
  }
  
  if (f2 != 0) {
    Frama_C_show_each_3(f2);
  } else {
    Frama_C_show_each_4(f2);
  }

  int i1 = ! (0. + 0.);
  int i2 = ! -0.;
  int i3 = ! (1. + 0.); // For comparison
}


/* Test for the equality domain: 0. and -0. are equal values, and this must
   not lead to unsound reductions. */
void equality_between_zeros () {
  /* Bypass the heuristics of the equality domain that avoids retaining
     equality between singletons. */
  float f1 = rand ? 0. : 10.;
  float f2 = rand ? -0. : 10.;
  float r = 0.;
  /* Despite the equality, -0. must still be in [f2], and so in [r]. */
  if (f1 == f2)
    r = f2;
}


void main (float f1, float f2) {
  main1(f1, f2);
  equality_between_zeros();
}
