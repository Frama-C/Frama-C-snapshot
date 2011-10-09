/* run.config
   GCC:
   STDOPT: +"-impact-pragma main"
   STDOPT: +"-impact-pragma main2" +"-main main2"
   STDOPT: +"-impact-pragma main3" +"-main main3"
   */

/*@ ghost int G; */

/*@ assigns G \from p; */
void p1 (int p);
void p2 (int);
int X;

void test (void) {
  if (X) p1(1); else p2(0);
}

/* ************************************************************************* */

void main (int x) {
  /*@ impact pragma stmt; */
  X = x;
  test ();
}

/* ************************************************************************* */

void call_test (void) {
  test ();
}

void main2(int x) {
  /*@ impact pragma stmt; */
  X = x;
  call_test ();
}

/* ************************************************************************* */

/*@ assigns G; */
void p3 (int);

void test3 (void) {
  if (X) p3(1); else p2(0);
}

void call_test3 (void) {
  test3 ();
}

void main3(int x) {
  /*@ impact pragma stmt; */
  X = x;
  call_test3 ();
}
