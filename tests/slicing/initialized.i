/* run.config
   OPT: -val-show-progress -slice-assert main -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check 
 **/

void main() {
  int x = 1;
  int y;

 L:
  x = 3;
  y = 2;
  //@ assert \initialized(&x);
  // assert !\initialized{L}(&y);  
}
