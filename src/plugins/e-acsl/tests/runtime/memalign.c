/* run.config
   COMMENT: Check aligned heap memory allocation
*/

#include <stdlib.h>

int posix_memalign(void **memptr, size_t alignment, size_t size);
void *aligned_alloc(size_t alignment, size_t size);

int main(int argc, const char **argv) {
  char **memptr = malloc(sizeof(void*));
  int res2 = posix_memalign((void**)memptr, 256, 15);

  char *p = *memptr;
  /*@assert \valid(p); */
  /*@assert \block_length(p) == 15; */
  /*@assert \freeable(p); */
  free(p);
  /*@assert ! \valid(p); */

  char *a;
  a = aligned_alloc(256, 12);
  /*@assert a == \null; */

  a = aligned_alloc(255, 512);
  /*@assert a == \null; */

  a = aligned_alloc(0, 512);
  /*@assert a == \null; */

  a = aligned_alloc(256, 512);
  /*@assert a != \null; */
  /*@assert \valid(a); */
  /*@assert \block_length(a) == 512; */
  /*@assert \freeable(a); */

  free(a);
  /*@assert ! \valid(a); */

  return 0;
}
