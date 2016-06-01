/* run.config*
STDOPT: +"-then -all-rounding-modes"
*/

//@ requires f1 >= 0. && f2 <= 0.;
void main(float f1, float f2) {
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
