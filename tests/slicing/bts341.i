/* run.config
   OPT: -val-show-progress -check -slice-assert main -journal-disable -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check
*/
int main (int c) {
  if (c)
    while (1) { ; }
  //@ assert c == 0;
  return c;
}
