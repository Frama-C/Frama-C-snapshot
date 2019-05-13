/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/
int f (int x) { return x; }
int g (int x) { return x; }
int h (int x) { return x; }

int i() { int y = 0; return y; }
int j() { int y = 0; return y; }
int k() { int y = 0; return y; }
