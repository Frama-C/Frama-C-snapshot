/* run.config*
   OPT: -aorai-automata tests/aorai/not_prm.ya -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test -main f -aorai-test-number @PTEST_NUMBER@ @PROVE_OPTIONS@
*/

int f(int x) {
  return x;
}
