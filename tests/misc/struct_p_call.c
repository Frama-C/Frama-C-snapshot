
typedef struct S {char v; int w;} U;


void f(U* G1) {
  G1->w = 0;
  G1->v = 1;
  return;
}


char main () {
  U H1;
  f(&H1);
  return H1.v;
}
