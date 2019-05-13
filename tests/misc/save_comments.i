/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -keep-comments
*/

int f() {
  int x = 0;
  /* Hello, I'm the f function */
  return x;
}
