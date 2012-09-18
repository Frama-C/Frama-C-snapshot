/* run.config


   OPT: -load-script tests/scope/zones.ml -val -journal-disable
*/

/* bin/viewer.opt -val tests/scope/zones.c */

int T [10];
struct Tstr { int a; int b; } S;
int X,Y,Z;

int simple (int x, int y, int z) {
  y = 3; //no need for y before
  x ++; //not used
  x = y + z;
  return x;
}

int array1 (int x, int y) {
  T[x] = 3;
  T[0] += y;
  return T[0];
}

int struct1 (int x, int y) {
  struct Tstr s;
  s = S; // lose precision : even if we need s.b after, we need S before
  s.a = x;
  s.b += y;
  return s.a;
}

int ctrl1 (int x, int y, int z) {
  int a;
  if (x) {
    a = y;
    goto Lt2; // to keep Lt2
    Lt2 : ;
  }
  else {
    a = z;
  }
  return a;
}

//================================================================

int Xf, Xg, Yf, Yg;

int f (int x, int y, int z) {
  Xf += x;
  Yf = z;
  return x + y;
}

int g (int a, int b, int c) {
  Xg += b;
  Yg = c;
  return a + b;
}

int caller (int cond, int t, int u, int v) {
  int x1 = 0, y1 = 0, z1 = 0, a1 = 0, b1 = 0, c1 = 0;
  int (*pf)(int, int, int) = cond ? &f : &g;
  f(x1, y1, z1);
  g(a1, b1, c1);
  return (*pf)(t, u, v);
}
//================================================================

int main (int x, int y, int z) {
  simple (x, y, z);
  array1 (x, y);
  struct1 (x, y);
  ctrl1 (x, y, z);

  caller (x, x, y, z);

  return 0;
}
