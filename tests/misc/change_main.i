/* run.config*
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -eva -main f -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -then-on change_main -main g -eva
*/

int f(int x) { return x; }
