/* run.config
   OPT: -aorai-automata tests/aorai/other.ya -aorai-test 1 -aorai-acceptance
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
