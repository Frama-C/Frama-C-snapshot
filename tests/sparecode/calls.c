/* run.config
   OPT: -sparecode-debug 1 -sparecode-analysis -journal-disable
   OPT: -slicing-level 2 -slice-return main -journal-disable -then-on 'Slicing export' -print
*/
int G;

int f (int x, int y) {
  G = y;
  return x;
}

int main (void) {
  int a = 1;
  int b = 1;

  f (0, 0); /* this call is useless : should be removed */
  a = f (a, b); /* the result of this call is useless */
  a = f (G + 1, b);

  G = 0; /* don't use the G computed by f */

  return a;
}
