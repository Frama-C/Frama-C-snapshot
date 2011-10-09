/* run.config
   EXECNOW: make -s tests/slicing/combine.opt
   CMD: tests/slicing/combine.opt
   OPT: -check -deps -journal-disable
*/

//@ assigns \result \from x;
int g (int x);

int f (int c, int x) {
  int y = c ? 1 : -1;
  int r;
  if (y < 0)
    r = x+y;
  else
    r = 0;
  r = g (r);
  return r;
}

int main (int x) {
  int r;
  if (x > 0)
    r = f (0, x);
  else
    r = f (1, x);
  return r;
}
