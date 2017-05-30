/*  run.config
OPT: -val-show-progress -slice-rd x -main f -slicing-project-name p -then-on 'p export' -print -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
*/

int f(void) {
  int x = 0;
  return x; // <- cannot be selected since x is a local variable
}
