/* run.config
   OPT: -val -slice-return main -then-on "Slicing export" -val -val-ilevel 16 -then-on "default" -val-ilevel 17 -then -val-ilevel 48
*/
// Test in particular that ilevel is by-project, even though it is an ocaml ref
volatile int v;
int i, j, k, l;

int main () {
  do { i = v; }
  while (! (0 <= i && i < 8));

  do { j = v; }
  while (! (0 <= j && j < 17));

  k = j;
  if (k == 16) k = 15;

  l = v;
  if (v) {
    //@ assert 0 <= l <= 4;
  } else {
    //@ assert 6 <= l <= 9;
  }
  Frama_C_show_each(l); // Possible problem with cache on offsetmap join

  return i+j+k+l;
}
