#include <string.h>
#include <stdlib.h>

int main() {
  /* ********** STACK ********** */
  int a = 111,
      b = 222;

  int *src[2],
      *dest[2];

  int size = sizeof(int*)*2;

  src[0] = &a;
  src[1] = &b;

  /* FIXME: memcpy should initialize,
     since so far it does not do the initialization by hand */
  dest[0] = NULL;
  dest[1] = NULL;

  /*@assert \valid(*src); */
  /*@assert \valid(*(src + 1)); */

  memcpy(&dest, &src, size);

  /*@ assert \valid(*dest); */
  /*@ assert \valid(*(dest + 1)); */

  memset(&dest, 0, size);

  /*@assert ! \valid(*dest); */
  /*@assert ! \valid(*(dest + 1)); */

  /* ********** HEAP ********** */
  int **p = malloc(size);
  int **q = malloc(size);

  *p = &a;
  *(p+1) = &a;

  /*@assert \valid(*p); */
  /*@assert \valid(*(p+1)); */
  /*@assert ! \valid(*q); */
  /*@assert ! \valid(*(q+1)); */

  *q = *(q+1) = NULL;
  memcpy(q, p, size);

  /*@assert  \valid(*q); */
  /*@assert  \valid(*(q+1)); */
  return 0;
}
