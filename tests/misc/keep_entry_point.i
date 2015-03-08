/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -main f -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -check -print
*/

static int f(void);

static int x;

static int y;

static int g() { return y++; }

static int f() { return x++; }
