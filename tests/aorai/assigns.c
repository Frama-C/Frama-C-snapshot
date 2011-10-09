/* run.config
   EXECNOW: make -s tests/aorai/aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/assigns.ya  -aorai-test 1 -load-module tests/aorai/aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/assigns_det.ya -aorai-test 1 -load-module tests/aorai/aorai_test.cmxs
*/

int X;

void f(void) { X++; }

/*@ assigns X;
  behavior foo:
  assigns X;
*/
int main () {
  //@ assigns X;
  X++;
  //@ assigns X;
  f();
  return X;
}
