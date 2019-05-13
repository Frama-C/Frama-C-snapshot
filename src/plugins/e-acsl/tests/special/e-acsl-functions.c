/* run.config
   COMMENT: test option -e-acsl-functions
   LOG: gen_@PTEST_NAME@.c
   OPT: -machdep gcc_x86_64 -e-acsl-functions f -e-acsl -then-last -load-script tests/print.cmxs -print -ocode tests/special/result/gen_@PTEST_NAME@.c -kernel-verbose 0 -eva-verbose 0 -eva
*/

/*@ requires \initialized(p);
  @ requires *p == 0;
  @ ensures \result == \old(*p); */
int f(int *p) {
  /*@ loop invariant 0 <= i <= 1; */
  for(int i = 0; i < 1; i++) ;
  return 0;
}

/*@ requires \initialized(p);
  @ requires *p == 1;
  @ ensures \result == \old(*p); */
int g(int *p) {
  /*@ loop invariant 0 <= i <= 1; */
  for(int i = 0; i < 1; i++) ;
  return 0;
}

int main(void) {
  int x = 0;
  int y = 0;
  f(&x);
  g(&y);
}
