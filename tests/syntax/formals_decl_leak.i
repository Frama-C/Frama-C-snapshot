/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -print -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs @PTEST_DIR@/@PTEST_NAME@_1.i
*/

void f(int x);

void g() { f(3); }
