/* run.config
   EXECNOW: make -s tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/seq_loop.ya -aorai-test 1 -load-module tests/aorai/Aorai_test.cmxs -aorai-acceptance -aorai-test-number @PTEST_NUMBER@
*/

void f() {}

void g() {}

//@ assigns \nothing;
int main(int c) {
  if (c<0) { c = 0; }
  if (c>5) { c = 5; }
  /*@ assert 0<=c<=5; */
  /*@ loop assigns c; */
  while (c) {
    f();
    g();
    c--;
  }
  return 0;
}

