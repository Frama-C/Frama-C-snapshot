/* run.config
   COMMENT: Behaviours of the \block_length E-ACSL predicate
*/

#include <stdlib.h>

int A[] = { 1, 2, 3, 4};
int *PA;

struct Zero { } ZERO;

int main(void) {
  /* Zero-sized blocks */
  struct Zero zero;
  /*@ assert \block_length(&ZERO) == 0; */
  /*@ assert \block_length(&zero) == 0; */

  /* Global memory */
  PA = (int*)&A;
  /*@ assert \block_length(&A[0]) == sizeof(A); */
  /*@ assert \block_length(A+3) == sizeof(A); */
  /*@ assert \block_length(PA) == sizeof(A); */
  PA++;
  /*@ assert \block_length(PA+1) == \block_length(A+1); */

  /* Stack memory [long blocks] */
  int a[] = { 1, 2, 3, 4};
  int *pa = (int*)&a;
  /*@ assert \block_length(&a[0]) == sizeof(a); */
  /*@ assert \block_length(a+3) == sizeof(a); */
  /*@ assert \block_length(pa) == sizeof(a); */
  pa++;
  /*@ assert \block_length(pa+1) == \block_length(a+1); */

  /* Stack memory [Short blocks] */

  long l = 4;
  char *pl = (char*)&l;
  /*@ assert \block_length(&l) == sizeof(long); */
  /*@ assert \block_length(pl) == sizeof(long); */
    /*@ assert \block_length(pl+7) == sizeof(long); */
  int *pi = (int*)&l;
  /*@ assert \block_length(pi) == \block_length(&l); */
  pi++;
  /*@ assert \block_length(pi) == \block_length(&l); */

  /* Heap memory [single segment] */
  size_t size = 12;
  char *p = malloc(size);
  /*@ assert \block_length(p) == size; */
  /*@ assert \block_length(p+11) == size; */
  p += 5;
  /*@ assert \block_length(p+5) == \block_length(p-5); */

  /* Heap memory [multiple segments] */
  size = 30*sizeof(long);
  long *q = malloc(size);

  /*@ assert \block_length(q) == size; */
  q += 4;
  /*@ assert \block_length(q) == size; */
}
