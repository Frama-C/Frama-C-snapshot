/* run.config
   EXECNOW: make -s tests/slicing/slice_no_body.opt
   CMD: tests/slicing/slice_no_body.opt
   OPT: -check -deps -lib-entry -main h -journal-disable
*/

int G;

int f (int a);

int g (int c) {
  int x = c+1;
  int y = c*2;
  if (c == 0)
    return f (x);
  else
    return y;
}

int h (void) {
  int a = f (1);
  int b = f (2);
  int c = f (3);
  G = f (4);
  if (G > 0)
    G = g (c);
  return g;
}
