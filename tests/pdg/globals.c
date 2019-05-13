/* run.config
  GCC:
  STDOPT: #"-load-module pdg -eva -out -input -deps -main g"
  STDOPT: #"-load-module pdg -eva -out -input -deps -main h"
  STDOPT: #"-load-module pdg -eva -out -input -deps -main f"
*/
struct Tstr;
extern int X;
extern struct Tstr S;

int f (struct Tstr * p) {
  return p ? X : 0;
}
int g (void) {
  return f (&S);
}

struct Tstr { int a; int b;};
struct Tstr2 { int a2; int b2; struct { int c2; } s2; };

int X = 3;
int *P = &X;

int h (int x) {
  struct Tstr2 s2;
  s2.a2 = x;
  s2.b2 = *P;
  return s2.a2 + s2.b2;
}
