/* run.config
EXECNOW: make -s tests/misc/Change_formals.cmxs
OPT: -load-module tests/misc/Change_formals.cmxs -then-on test -print
OPT: -load-module tests/misc/Change_formals.cmxs -check -then-on test -print
*/

int f(int x) { return x; }
