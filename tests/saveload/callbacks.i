/* run.config
   EXECNOW: LOG callbacks_initial.res LOG callbacks_initial.err BIN callbacks.sav ./bin/toplevel.opt tests/saveload/callbacks.i -inout-callwise -out -calldeps -main main1 -save ./tests/saveload/result/callbacks.sav > ./tests/saveload/result/callbacks_initial.res 2> ./tests/saveload/result/callbacks_initial.err
   OPT: -load ./tests/saveload/result/callbacks.sav -main main2 -then -main main3
*/

/* This tests whether the callbacks for callwise inout and from survive after
   a saveload or a -then */

void f(int *p) {
  *p = 1;
}

int x, y;

void g1() {
  f(&x);
}


void g2() {
  f(&y);
}

void main1() {
  g1();
  g2();
}

void main2() {
  g1();
  g2();
}

void main3() {
  g1();
  g2();
}
