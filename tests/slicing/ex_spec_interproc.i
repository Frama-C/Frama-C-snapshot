/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   CMD: @frama-c@ -load-module tests/slicing/libSelect.cmxs -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: @EVA_OPTIONS@ -deps -journal-disable
*/

int X, Y;

int g (int u, int v, int w) {
  u++; v++; w++;
  X = u;
  Y = u+v;
  return w;
}

int Z;

int f (int a, int b, int c, int d, int e) {
  int r;
  a++; b++; c++; d++; e++;
  r = g (a, b, c);
  Z = g (r, d, e);
  return X;
}

int I, J, K, L, M;

int main (void) {
  int res;
  I = 0;
  J = 0;
  K = 0;
  L = 0;
  M = 0;
  res = f (I, J, K, L, M);
  return res;
}
