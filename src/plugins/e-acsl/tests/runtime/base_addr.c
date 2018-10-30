/* run.config
 * COMMENT: Behaviours of the \base_addr E-ACSL predicate
*/

#include <stdlib.h>

int A[] = { 1, 2, 3, 4};
int *PA;

int main(void) {
  /* Global memory */
  PA = (int*)&A;
  /*@ assert \base_addr(&A[0]) == \base_addr(&A); */
  /*@ assert \base_addr(&A[0]) == \base_addr(PA); */
  /*@ assert \base_addr(A+3) == \base_addr(PA); */
  PA++;
  /*@ assert \base_addr(PA) == \base_addr(&A[0]); */
  /*@ assert \base_addr(PA+2) == \base_addr(A+3); */

  /* Stack memory [long blocks] */
  int a[] = { 1, 2, 3, 4 };
  int *pa;
  pa = (int*)&a;

  /*@ assert \base_addr(&a[0]) == \base_addr(&a); */
  /*@ assert \base_addr(&a[0]) == \base_addr(pa); */
  /*@ assert \base_addr(a+3) == \base_addr(pa); */
  pa++;
  /*@ assert \base_addr(pa) == \base_addr(&a[0]); */
  /*@ assert \base_addr(pa+2) == \base_addr(&a[0]); */

  /* Stack memory [Short blocks] */
  long l = 4;
  char *pl = (char*)&l;
  /*@ assert \base_addr(&l) == \base_addr(pl); */
  /*@ assert \base_addr(pl+2) == \base_addr(&l); */
  short *pi = (short*)&l;
  pi++;
  pl++;
  /*@ assert \base_addr(pi) == \base_addr(pl); */
  /*@ assert \base_addr(pl) == \base_addr(&l); */

  /* Heap memory [single segment] */
  char *p = malloc(12);
  char *pd = p;
  /*@ assert \base_addr(p) == \base_addr(pd); */
  /*@ assert \base_addr(p+1) == \base_addr(pd+5); */
  /*@ assert \base_addr(p+11) == \base_addr(pd+1); */
  p += 5;
  /*@ assert \base_addr(p+5) == \base_addr(pd); */
  /*@ assert \base_addr(p-5) == \base_addr(pd); */

  /* Heap memory [multiple segments] */
  long *q = malloc(30*sizeof(long));
  long *qd = q;

  /*@ assert \base_addr(q) == \base_addr(qd); */
  q++;
  /*@ assert \base_addr(q) == \base_addr(qd); */
  q += 2;
  /*@ assert \base_addr(q) == \base_addr(qd); */
  q += 4;
  /*@ assert \base_addr(q) == \base_addr(qd); */
}
