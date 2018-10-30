/* run.config
  COMMENT: Check violations related to accessing an allocated memory block
  COMMENT: through a pointer to another block
*/

#include <stdlib.h>
#include <stdint.h>

#define ADDROF(_a) ((uintptr_t)_a)

int A = 1,
    B = 2,
    C = 3;

int main(int argc, char **argv) {
  int *p = NULL,
      *q = NULL;

  int a = 1,
      b = 2,
      c = 3;

  p = &b;
  /*@assert \valid(p); */
  /* `p` points to `b`, `p+1` accesses either `a` or `c` */
  /*@assert ! \valid(p+1); */

  p = &B;
  /*@assert \valid(p); */
  /* `p` points to `B`, `p+1` accesses either `A` or `C` */
  /*@assert ! \valid(p+1); */

  char *pmin = (char*)malloc(sizeof(int));
  char *pmax = (char*)malloc(sizeof(int));

  /* Since `pmin` is allocated before `pmax` it is likely that the start
   * address of `pmin` is less than the start address of `pmax`, still,
   * just in case, make sure it is true and swap the addresses otherwise. */
  if (ADDROF(pmin) > ADDROF(pmax)) {
    char *t = pmin;
    pmin = pmax;
    pmax = t;
  }

  *pmin = 'P';
  *pmax = 'L';

  int diff = (uintptr_t)pmax - (uintptr_t)pmin;
  /*@assert \valid(pmin); */
  /*@assert \valid(pmax); */
  /* Access `pmax` through `pmin` */
  /*@assert ! \valid(pmin + diff); */
  /* Access `pmin` through `pmax` */
  /*@assert ! \valid(pmax - diff); */
  return 0;
}
