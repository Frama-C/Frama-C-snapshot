/* run.config
   OPT: -val -slice-return main  -then-on "Slicing export" -val -val-ilevel 16 -then-on "default" -val-ilevel 17 -then -val-ilevel 39
*/

volatile int v;
int i, j, k;

int main () {
  do { i = v; }
  while (! (0 <= i && i < 8));

  do { j = v; }
  while (! (0 <= j && j < 17));

  k = j;
  if (k == 16) k = 15;

  return i+j+k;
}
