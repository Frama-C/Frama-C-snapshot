/* run.config
   OPT: -journal-disable -load-script tests/scope/bts971.ml -then -main main2
*/

/* bug 971: */
volatile foo;
int v;

void f1 () {
  v += 1;
}

void f () {
  f1 ();
}

void g1 () {
  v += 2;
  v += 3;
}

void g () {
  g1 ();
}

void main (int c) {
  v += 0;
  while (c) {
    if (foo) {f ();};
    if (foo) {g ();};
  }
}

/* bug 972 */
typedef struct {
  int a;
  int b;
} ts;

ts t[10];

void init() {
  t[1].a = 1;
  t[1].b = 2;
}

int main2 () {
  init();
  return t[1].a;
}
