/* run.config
DONTRUN: main test is at merge_union.c
*/

#pragma noalign
#include "merge_union.h"

int f(un* u);

int g(un* u) {
  f(&G1.u);
  return 0;
}
