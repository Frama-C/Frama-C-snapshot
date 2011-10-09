/* run.config
   EXECNOW: make -s tests/aorai/aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/not_prm.ya -aorai-test 1 -aorai-acceptance -load-module tests/aorai/aorai_test -main f
*/

int f(int x) {
  return x;
}
