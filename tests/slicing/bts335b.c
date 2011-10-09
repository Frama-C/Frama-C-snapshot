/* run.config
   OPT: -check -slice-return main -calldeps -slicing-level 3 -slicing-verbose 2 -journal-disable -then-on 'Slicing export' -print
*/

int X, Y;
int g(int c, int x, int y, int z) {
  X = z ;
  if (c == 1)
    X = x;
  if (c == 2)
    X = y;
  Y = X ;
  return X;
}
int f(int c, int x, int y, int z) {
  z++;
  return g(c,x,y,z);
}
int main(int v, int w, int a, int b, int i, int j) {
  int r = f(1, v, a, i) ;
  j++;
  r += g(2, w, b, j) ;
  return r ;
}
