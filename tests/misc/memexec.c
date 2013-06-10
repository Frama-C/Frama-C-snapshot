/* run.config
   STDOPT:  +"-rte-select fbug -rte -memexec-all"
*/

int x1, y1, z1; volatile int c;

void f11() {
  x1 = 1;
}

void f1 () {
  f11();
  f11();
  f11();
  x1 = 0;
  f11();
  x1 = 1;
  f11();
  x1 = 2;
  f11();
  f11();
}

void f2 () {
}

void f3 () {
}

int *p;

int fbug() {
  return *p;
}

void bug() {
  p = 0;
  int x;
  if (c)
    fbug();
  p = &x;
  fbug();
}

void main () {
  f1 ();
  f2 ();
  f3 ();
  bug();
}
