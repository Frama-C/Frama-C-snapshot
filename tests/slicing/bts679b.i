/*  run.config
OPT: -val-show-progress -slice-assert main -then-on "Slicing export" -print -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -no-deps
*/

int X = 1 ;

int main(void) {
  int y;
L: y = 0;
   X++;
  //@ assert X > \at(X,L);
  return X;
}
