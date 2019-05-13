/* run.config
   COMMENT: Test option -cg-function-pointers
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -cg-function-pointers -no-autoload-plugins -load-module eva,@PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -cg-no-services -cg-function-pointers -no-autoload-plugins -load-module eva,@PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -cg-no-function-pointers -no-autoload-plugins -load-module eva,@PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -cg-no-services -cg-no-function-pointers -no-autoload-plugins -load-module eva,@PTEST_DIR@/@PTEST_NAME@.cmxs
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
