/* run.config
OPT: -print @PTEST_DIR@/merge_logic_globals_2.c -cpp-extra-args="-I@PTEST_DIR@"
*/
#include "merge_logic_globals.h"

int main() { test(); /*@ assert p((int)li); */ }
