/* run.config
   STDOPT: +"-main main1 -then -slevel 3 -main main2"
*/

int foo() {
  return 1;
}

volatile int c;

void main() {
  int x;
  int **p;
  int *q = &x;

  if (c) p = &q;
  *(*p) = foo();
}

void main1() {
  main();
}

void main2() {
  main();
}
