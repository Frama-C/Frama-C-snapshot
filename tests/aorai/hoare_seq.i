/* run.config
EXECNOW: make -s tests/aorai/aorai_test.cmxs
OPT: -aorai-automata tests/aorai/hoare_seq.ya -aorai-test 1 -aorai-acceptance -load-module tests/aorai/aorai_test
*/

void f(void) { }

/*@ behavior bhv:
    assumes c > 0;
    ensures \result == 0;
*/
int main(int c) {
  if (c <= 0) { f (); }
  return 0;
}
