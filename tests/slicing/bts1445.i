/*  run.config
OPT: -slice-calls main -then-on "Slicing export" -print -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
OPT: -slice-calls f -main f -then-on "Slicing export" -print -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
*/
int x = 0;

int main() {
  while(1) 
    x=0;
  return x + 1;
}

int f() {
  while(1) 
    x=0;
  return x + 1;
}
