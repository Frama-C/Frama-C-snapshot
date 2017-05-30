/* run.config
   OPT: -val-show-progress -slice-calls main -journal-enable -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
*/
// one bug about JOURNALIZATION and another one about slicing CALLS TO MAIN function.
double d1, d2, d3;
int x1, x2, x3;
int main2 (void) {
  d1 = d2 * d3;
  x1 = x2 * x3;
  return 1;
}

int main (void) {
  return main2();
}
