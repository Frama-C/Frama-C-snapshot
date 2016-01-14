/* run.config
  GCC:
  STDOPT: #"-main g"
  STDOPT: #"-main h"
*/
int * f (int *r) {
  return r;
}

int * p, *q;
int x,y,z;

void g() {
  p = f(&x);
}

void h() {
  q = f(&y);
}
