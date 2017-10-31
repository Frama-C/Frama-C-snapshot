/* run.config
   COMMENT: Test option -cg-function-pointers
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -cg-function-pointers -load-module @PTEST_DIR@/@PTEST_NAME@
   OPT: -cg-no-services -cg-function-pointers -load-module @PTEST_DIR@/@PTEST_NAME@
   OPT: -cg-no-function-pointers -load-module @PTEST_DIR@/@PTEST_NAME@
   OPT: -cg-no-services -cg-no-function-pointers -load-module @PTEST_DIR@/@PTEST_NAME@
*/

int (*fptr)(int);

int f(int x) { return x; }
int g(int x) { return x-1; }

int main(void) {
  int x = 0;
  fptr = f;
  x = (*fptr)(1);
  fptr = g;
  x = (*fptr)(1);
  return x;
}
