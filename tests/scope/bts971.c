/* run.config
   OPT: -journal-disable -load-script tests/scope/bts971.ml
*/


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
