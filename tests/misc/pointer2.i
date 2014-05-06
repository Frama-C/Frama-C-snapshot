/* run.config
  GCC:
  OPT: -val -deps -out -input  -main g -journal-disable
  OPT: -val -deps -out -input  -main h -journal-disable
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
