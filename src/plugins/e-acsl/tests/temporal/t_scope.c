/* run.config
   COMMENT: Temporal analysis with respect to scopes
*/
#include <stddef.h>

int main() {
  int *p = NULL,
      *q = NULL;
  {
    int i = 9;
    p = &i;
    q = p;
  }

  /*@assert ! \valid(p); */
  /*@assert ! \valid(q); */

  {
    int j = 8;
    p = &j;
    /*@assert \valid(p); */
    *p = 1;
    /* `q` now may point to `j`, bit not necessarily */
    /*@assert ! \valid(q); */
    /*@assert \valid(&j); */
  }

  /* Example from Arvid's report (Listing 3.12) */
  int len = 3;
  p = NULL,
  q = NULL;
  while (len) {
    int a;
    /*@assert ! \valid(p); */
    q = &a;
    p = q;
    /*@assert \valid(p); */
    len--;
  }
  return 0;
}
