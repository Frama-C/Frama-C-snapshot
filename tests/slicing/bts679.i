/*  run.config
OPT: -val-show-progress  -slice-return main -then-on "Slicing export" -print  -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  
*/
void f(void) { return; }
int X = 1 ;
int main(void) {
 call: f();
  //@ assert X == \at(X,call);
  return X; 
}
