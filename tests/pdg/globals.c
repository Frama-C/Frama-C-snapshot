/* run.config
  GCC:
  OPT: -val -deps -out -input  -main g -journal-disable -pdg-print -pdg-verbose 2
  OPT: -val -deps -out -input  -main h -journal-disable -pdg-print -pdg-verbose 2
  OPT: -val -deps -out -input  -main f -journal-disable -pdg-print -pdg-verbose 2
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
