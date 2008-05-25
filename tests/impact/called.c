/* run.config
   OPT: -impact-pragma g -impact-print -lib-entry g
   */

int X;
int f(int x, int y) { X = x; return y; }
void g() {
  int a, b, c, d;
  b = 0;
  /*@ impact pragma stmt; */
  a = 0;
  c = f(a,b);
  d = X;
  c = f(a,d);
}
