/* run.config
   EXECNOW: make -s tests/slicing/horwitz.cmxs
   CMD: @frama-c@ -load-module tests/slicing/libSelect.cmxs -load-module tests/slicing/horwitz.cmxs
   OPT: -check -deps -slicing-level 0 -journal-disable
*/

/* bin/toplevel.opt -deps -val tests/slicing/horwitz.c */
/* bin/toplevel.opt -deps -pdg-debug -pdg tests/slicing/horwitz.c */
/* cf aussi tests/slicing/horwitz.ml */

int add (int a, int b) {
  return a+b;
}
void incr (char * pi) {
  *pi = add (*pi, 1);
}
int A (int x, char * py) {
  x = add (x, *py);
  incr (py);
  /*@ slice pragma expr x;*/
  return x;
}
int main (void) {
  int s = 0;
  char i = 1;
  while (i < 11) {
    s = A (s, &i);
  }
  return s;
}
