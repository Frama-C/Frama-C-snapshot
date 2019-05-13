/* run.config
   COMMENT: test option -e-acsl-instrument
   LOG: gen_@PTEST_NAME@.c
   OPT: -machdep gcc_x86_64 -e-acsl-instrument="@@all,-uninstrument1,-uninstrument2" -e-acsl -then-last -load-script tests/print.cmxs -print -ocode tests/special/result/gen_@PTEST_NAME@.c -kernel-verbose 0 -eva-verbose 0 -eva
*/

int uninstrument1(int *p) {
  *p = 0;
  return 0;
}

/*@ requires \valid(p); */
int uninstrument2(int *p) {
  { int *q = p;
    *p = 0; 
    goto L;
  }
 L:
  return 0;
}

int instrument1(int *p) {
  *p = 0;
  return 0;
}

/*@ requires \valid(p); */
int instrument2(int *p) {
  { int *q = p;
    *p = 0; 
    goto L;
  }
 L:
  return 0;
}

int main(void) {
  int x;
  int y = 0;
  instrument1(&x);
  uninstrument1(&x);
  instrument2(&x);
  uninstrument2(&x);
  /*@ assert \initialized(&x); */
  /*@ assert \initialized(&y); */
}
