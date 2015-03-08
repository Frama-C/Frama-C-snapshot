/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/deterministic.ya -aorai-test 1 -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/

int X;
int Y;

void g(int x) {
  Y=x;
}

int f(int x) {
  X=x;
  g(X);
  X++;
  g(X);
  return 0;
}

int real_main (int c) {
  if (c) f(4);
  return 0;
}

int main (int c) {
  return real_main(c);
}
