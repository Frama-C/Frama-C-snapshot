/* run.config*
   GCC:
   STDOPT: #"-val-subdivide-non-linear 15"
*/

int t[8] = {1, 2, 4, 8, 16, 32, 64, 128};

void main() {
  int d;
  for (int i=0; i<7; i++) {
    if (t[i] - t[i+1] >= 0) {
      d = t[i] - t[i+1];
      Frama_C_show_each_bug(i, d);
    } else {
      d = t[i] - t[i+1];
      Frama_C_show_each_ok(i, d);
    }
  }
}
