/* run.config
DONTRUN: main test is at tests/syntax/typedef_multi_1.c
*/

#include "tests/syntax/typedef_multi.h"

void g() { 
  /*@ loop invariant x<=(3+2); */ 
  while (x<y) x++; }
