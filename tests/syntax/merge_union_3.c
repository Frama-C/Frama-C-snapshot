/* run.config
DONTRUN: main test at merge_union.c
*/
#pragma noalign
#include "merge_union.h"

int g(un* u);

int main(un* u) {
  g(&G1.u);
  return 0;
}
