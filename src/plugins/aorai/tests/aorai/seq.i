/* run.config
   EXECNOW: make -s tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/seq.ya -aorai-test 1 -load-module tests/aorai/Aorai_test.cmxs -aorai-acceptance -aorai-test-number @PTEST_NUMBER@
 */

void f() { }

void g() { }

int main(int c) {
  if (c) f();
  g();
  if (c) g();
  return 0;
}
