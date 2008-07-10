
int a, b;
typedef int t, u;
void f1() { a * b; }
void f2() { t * u; }
void f3() { t * b; }
void f4() { int t; t * b; }
void f5(t u, unsigned t) {
  switch ( t ) {
  case 0: if ( u )
    default: return;
  }
}

