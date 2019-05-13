/* run.config
   EXECNOW: make -s @PTEST_DIR@/typedef_multi.cmxs
   OPT: -load-module @PTEST_DIR@/typedef_multi tests/syntax/typedef_multi_2.c
*/

#include "tests/syntax/typedef_multi.h"

void f () {  while(x<y) x++; }
