/* run.config
  OPT: -val-show-progress  -main h -slice-return h -slicing-level 1 -journal-disable -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i 
 */

int X ;
typedef void (*PTF)(int);

void f1(int x) {
  X = x ;
}
void f2 (int y) ; /* no source */

PTF ptf = 0 ;

void g(int arg) {
  ptf = (arg > 0 ? &f1 : &f2) ;
  (*ptf)(arg) ;
}

PTF h (int a, int b) {
  if (b) {
    ptf = &g;
    (*ptf)(a);
    }
  return ptf;
}
