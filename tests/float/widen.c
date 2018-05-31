/* run.config*
  STDOPT: #"-warn-special-float non-finite -wlevel 3"
  STDOPT: #"-warn-special-float none -wlevel 3"
*/

volatile int rand;

/* Tests the widening near infinity. See gitlab issue #493. */
void main1 () {
  double max = 1.;
  float f = 1.;
  double d = 1.;
  while (rand) {
    max *= 2.;
  }
  while (rand) {
    if (d >= 3.)
      d = max;
    else
      d += 1.;
  }
  /* In -warn-special-float non-finite, d should not reach infinity.
     In -warn-special-float none, d should reach infinity. */
  Frama_C_show_each_double_inf(d);
  while (rand) {
    if (f >= 3.)
      f = max;
    else
      f += 1.;
  }
  /* In -warn-special-float non-finite, f should not reach infinity.
     In -warn-special-float none, f should reach infinity.
     In no case f should have 64 bits finite bounds. */
  Frama_C_show_each_float_inf(f);
}

/* Tests the widening near -0. */
void main2 () {
  double d = 3.;
  while (d > 0.) {
    d -= 1.;
    d = (d <= 0. ? -0. : d);
  }
  /* d must contain -0.0. */
  Frama_C_show_each(d);
}


void main () {
  main1();
  main2();
}
