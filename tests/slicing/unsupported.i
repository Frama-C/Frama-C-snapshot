/* run.config
   OPT:  -slice-return main -journal-disable -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  
   OPT: -sparecode
*/
int main() {
  int t[10] = {0, 1, 2};
  return t[5]+t[2];
}
