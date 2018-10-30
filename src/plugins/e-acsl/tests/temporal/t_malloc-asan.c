/* run.config
   COMMENT: Temporal analysis with respect dynamic memory allocation.
   COMMENT: malloc-free-malloc errors
   COMMENT: This test is a modification aiming targeting AddressSanitizer and
   COMMENT: Valgrind tools who are not capable of catching this bug
*/

#include <stdlib.h>
#include <stdio.h>

#define MB (1024*1024)

int main(void) {
  int *p, *q;
  int counter = 0;
  size_t limit = 10000;

  p = (int*)malloc(MB);
  q = p;

  free(p);

  /* Allocate and de-allocate a chunk of
    memory until allocator reuses address */
  while (counter < limit) {
    p = (int*)malloc(MB);
    counter++;
    if (p != q)
      free(p);
    else {
      printf("Same address %p in %d steps\n", p, counter);
      break;
    }
    p = NULL;
  }

  if (p) {
    *q = 1; /* temporally invalid */
    *p = 2;
  }

  free(p);
  return 0;
}

