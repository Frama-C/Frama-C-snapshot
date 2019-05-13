/* run.config*
  STDOPT: +"-eva-no-alloc-returns-null"
*/

#include <stdlib.h>
#include <string.h>

volatile v;

static void copy(void *dst_, void *src_, size_t off, size_t len)
{
  char *dst = dst_;
  char *src = src_;
  memcpy(dst + off, src + off, len);
}

// Bug reported by Trust-in-Soft
int main1(void) {
  int *t[2];

  /*@ slevel 2; */
  for (int i = 0; i < 2; i++)
    t[i] = malloc(0x80);

  int *p;
  size_t n = sizeof(void *);
  copy(&p, &t[1], 0, 1);
  copy(&p, &t[0], 1, n - 1);
  *p = 42; /* p should not be a valid pointer */
  int r = *p;
  return r;
}


void main2() { // Test performance of iterating on strong malloced variables
  int t[1000];
  int i = malloc(sizeof(int));

  //@ slevel 10000;
  for (i = 0; i < 800; i++) {
    t[i] = i;
  }
}

/* Tests pointer subtraction and comparison on weak bases. */
void main3() {
  int *p, *q, *r;
  /* For the test to be meaningful, q and r must point to the same weak base
     at the end of the loop. */
  for (int i = 0; i < 10; i++) {
    p = malloc(4);
    if (i % 2)
      q = p;
    else
      r = p;
  }
  /* At the end of the loop, q and r point on distinct allocated memory, so:
     - the subtraction must lead to a differing_blocks alarm;
     - the relational comparison must lead to a pointer_comparable alarm;
     - the equality must not be true (this also tests the backward
       propagators for the comparison). */
  int d = q - r;
  int cmp = q < r;
  int eq;
  if (q == r)
    eq = 1;
  else
    eq = 0;
}

void main() {
  main1();
  main2();
  main3();
}
