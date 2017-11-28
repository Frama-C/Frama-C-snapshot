/* run.config
DONTRUN: main test is run through merge_unused.c
*/

#include "merge_unused.h"

void f() {
  int j = G2;
  int k = G3.i;
}
