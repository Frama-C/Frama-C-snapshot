/* run.config
 OPT: -val-show-progress -slice-return main -journal-disable -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 OPT: -val-show-progress -slice-pragma main -journal-disable -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 OPT: -val-show-progress -sparecode-analysis -journal-disable
*/

struct {int a; int ab; int b; int c ; int d;} S;
int X, Y;
void g (void) {
  S.a = 1;
  S.ab = 0;
  S.b = 2; /* here, better can be done ! */
  S.d = 4;
}
int main (void) {
  g();
  //@  slice pragma expr S.b;
  S.ab = 1; /* so that S.ab is sparecode in g() */
  return S.a ;
}
