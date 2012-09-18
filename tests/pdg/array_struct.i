/* run.config
  OPT: -pdg -pdg-print -journal-disable -pdg-verbose 2
*/

typedef struct {
  int a;
  int b;
} ts;

ts t[100];

void f(int c) {
  t[c].a=t[c].a;
  t[c].b=t[c].b;
}

void main(int c) {
  f(c);
  f(c);
}
