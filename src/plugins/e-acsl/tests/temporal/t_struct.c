/* run.config
   COMMENT: Several basic cases involving assignments of structs
*/

/* Data types and typedefs {{{ */
#include <stdlib.h>

struct temporal_t {
  char *p;
  char *q;
};

struct larger_t {
  char *p;
  char *q;
  struct temporal_t t;
};

typedef struct temporal_t temporal;
typedef struct larger_t larger;
/* }}} */

int main(void) {
  int  a = 1,
       b = 2;
  temporal t1, t2, *tp, tarr[2];
  larger l, *lp, larr[2];

  t1.p = &a;
  t1.q = t1.p;
  /*@assert \valid(t1.p) && \valid(t1.q);  */
  /*@assert !\valid(t2.p) && !\valid(t2.q);  */

  t2 = t1;
  /*@assert \valid(t2.p) && \valid(t2.q);  */

  t2.p = NULL;
  t2.q = malloc(4);
  /*@assert ! \valid(t2.p) && \valid(t2.q);  */

  l.t = t2;
  /*@assert ! \valid(l.t.p) && \valid(l.t.q); */

  lp = &l;
  /*@assert ! \valid(lp->t.p); */

  tarr[0] = t2;
  /*@assert ! \valid(tarr[0].p) && \valid(tarr[0].q) ; */

  larr[0] = l;
  /*@assert ! \valid(larr[0].t.p) && \valid(larr[0].t.q) ; */
  return 0;
}
