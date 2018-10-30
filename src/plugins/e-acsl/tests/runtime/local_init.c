/* run.config
   COMMENT: test of a local initializer which contains an annotation
   LOG: gen_@PTEST_NAME@.c
   OPT: -e-acsl-prepare -val -lib-entry -then -e-acsl -then-last -load-script tests/print.cmxs -print -ocode @PTEST_DIR@/result/gen_@PTEST_NAME@.c
   EXEC: ./scripts/testrun.sh @PTEST_NAME@ runtime "" "--frama-c=@frama-c@ -F -val -F -lib-entry --then --e-acsl-extra=-no-lib-entry --e-acsl-extra=-no-val"
*/

int X = 0;
int *p = &X;

int f(void) {
  int x = *p; // Eva add an alarms on this statement
  return x;
}

int main(void) {
  f();
  return 0;
}
