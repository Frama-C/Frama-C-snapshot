/* run.config
   OPT: -eva @EVA_OPTIONS@ -then -scf -then-on propagated -scf @EVA_OPTIONS@
*/

void f(int *x, int *y, void (*p)(int *x, int *y)) {
  (*x)++;
  (*y)++;
  p(x, y);
}

void g(int *x, int *y, void (*p)(int *x, int *y)) {
  (*x)++;
  (*y)++;
  p(x, y);
}

extern int X;

int Y = -42;

void h(int *x, int *y) {
  *x += 2;
  *y += 5;
}

int main () {
  f(&X, &Y, h);
  g(&X, &Y, h);
  return Y;
}
