/* run.config
   OPT: -val-show-progress -check -slice-value r -journal-disable -slevel 101 -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
 **/

int r=1;

int main() {
  for (int i = -100; i < 100; i++) {
    if (i != 0)
      if (i)
        r += 1;
  }
  return r;
}
