/* run.config
DONTRUN: main test is merge_logic_globals_1.c
*/

#include "merge_logic_globals.h"

int f() { t x; x.n = i; i--; /*@ assert p(x.n); */ return x.n; }
