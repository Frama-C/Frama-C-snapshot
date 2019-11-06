/* run.config_ci
   COMMENT: test of a local initializer which contains an annotation
   LOG: gen_@PTEST_NAME@.c
   STDOPT: #"-lib-entry -eva -e-acsl-prepare -e-acsl-share ./share/e-acsl -then -no-lib-entry"
*/

int X = 0;
int *p = &X;

int f(void) {
  int x = *p; // Eva's alarm in -lib-entry on this local initializer
  return x;
}

int main(void) {
  f();
  return 0;
}
