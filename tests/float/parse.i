/* run.config*
   STDOPT: #"-warn-decimal-float all -float-hex"
*/

volatile v;

int main() {
  if (v) {
    double d = 0.0E9999999999999999999;
    Frama_C_show_each(d, "reached");
  }

  if (v) {
    double d = 0.0E-9999999999999999999;
    Frama_C_show_each(d, "reached");
  }

  if (v) {
    double d1 = 0e500;
    double d2 = 0.0e500;
    Frama_C_show_each(d1, d2, "reached");
  }

  if (v) {
    double d = 0.00000000000000000000000000000000000000001e310;
    Frama_C_show_each(d, "reached");
  }

  if (v) {
    double d = 0.0000001E9999999999999999999;
    Frama_C_show_each("unreached");
  }

  // Shows several issues with long double in Eva, but should at least not crash.
  if (v) {
    long double l = 0x1p32767L;
    int long_double = (int)l;
  }
}
