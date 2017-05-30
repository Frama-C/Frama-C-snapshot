/* run.config
   OPT: -val-show-progress -check -deps -slice-return main -journal-disable -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check -no-deps
*/
int main() {
  volatile int a=0,b,c;
  if (a)
    {a = 1;

  while (1) {
    a++;
    };
  return 0;}
}
