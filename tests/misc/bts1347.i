/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT:  @EVA_OPTIONS@ -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -then -report
*/
int f(int *x) { return *x; }
int g(int *x) { return *(x++); }
