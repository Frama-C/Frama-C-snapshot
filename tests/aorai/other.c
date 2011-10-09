/* run.config
   EXECNOW: make tests/aorai/aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/other.ya -aorai-test 1 -aorai-acceptance -load-module tests/aorai/aorai_test.cmxs
*/

int x=0;

void f (void) { x=3; }

void g (void) { x=4; }

int main () {
  f();
  g();
  f();
  g();
  return x;
}
