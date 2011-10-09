/* run.config
   EXECNOW: make -s tests/aorai/aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/seq.ya -aorai-test 1 -load-module tests/aorai/aorai_test.cmxs -aorai-acceptance
 */

void f() { }

void g() { }

int main(int c) {
  if (c) f();
  g();
  if (c) g();
  return 0;
}
