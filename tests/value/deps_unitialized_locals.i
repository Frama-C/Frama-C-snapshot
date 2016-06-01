/* run.config*
   STDOPT: #"-calldeps"
*/
volatile maybe;

typedef struct {
  short ts;
  int ti;
} typ;

typ v1, v2;
int v;

void f() {
  typ ty = {1, 2};
  v1 = ty; // Dependency for the padding between ts and ti
}

void g() {
  int i;
  if (maybe)
    i = 1;
  v = i;
}

void h(int i) {
  if (maybe)
    i = 1;
  v = i;
}

void main() {
  f();
  v2 = v1; // Dependency must not leak there

  g();

  int x = 1;
  h(x);
}
