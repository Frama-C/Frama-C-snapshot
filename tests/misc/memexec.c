/* run.config
   STDOPT:  +"-memexec-all"
*/

int x1, y1, z1;

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


void main () {
  f1 ();
  f2 ();
  f3 ();
}

