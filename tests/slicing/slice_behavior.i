/* run.config
   OPT: -val-show-progress -check -val -slice-assert f -slicing-level 0 -journal-disable -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-val
*/
/*@ requires a > 0; */
int f(int a) {
  int b = 2 * a;
  /*@ assert a < b; */
  return 42;
}

int main () {
  f(10);
  return 0;
}
