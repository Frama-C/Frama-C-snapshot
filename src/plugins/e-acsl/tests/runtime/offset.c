/* run.config
   COMMENT: Behaviours of the \offset E-ACSL predicate
*/

#include <stdlib.h>

int A[] = { 1, 2, 3, 4};
int *PA;

int main(void) {
  /* Global memory */
  PA = (int*)&A;
  /*@ assert \offset(&A[0]) == 0; */
  /*@ assert \offset(A+3) == 12; */
  /*@ assert \offset(PA) == 0; */
  PA++;
  /*@ assert \offset(PA+1) == 8; */

  /* Stack memory [long blocks] */
  int a[] = { 1, 2, 3, 4};
  /*@ assert \offset(&a[0])   == 0; */
  /*@ assert \offset(a+1) == 4; */
  /*@ assert \offset(a+3) == 12; */

  /* Stack memory [Short blocks] */
  long l = 4;
  char *pl = (char*)&l;
  /*@ assert \offset(&l) == 0; */
  /*@ assert \offset(pl) == 0; */
  /*@ assert \offset(pl+1) == 1; */
  /*@ assert \offset(pl+7) == 7; */
  int *pi = (int*)&l;
  /*@ assert \offset(pi) == 0; */
  pi++;
  /*@ assert \offset(pi) == 4; */

  /* Heap memory [single segment] */
  char *p = malloc(12);
  /*@ assert \offset(p) == 0; */
  /*@ assert \offset(p+1) == 1; */
  /*@ assert \offset(p+11) == 11; */
  p += 5;
  /*@ assert \offset(p+5) == 10; */
  /*@ assert \offset(p-5) == 0; */

  /* Heap memory [multiple segments] */
  long *q = malloc(30*sizeof(long));

  /*@ assert \offset(q) == 0; */
  q++;
  /*@ assert \offset(q) == sizeof(long); */
  q += 2;
  /*@ assert \offset(q) == sizeof(long)*3; */
  q += 4;
  /*@ assert \offset(q) == sizeof(long)*7; */
}
