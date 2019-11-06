/* run.config
   COMMENT: Check that deleting statements before goto jumps takes into
   COMMENT: account variable declarations given via local inits
*/
#include <stdio.h>

#define describe(lab) \
  printf("t is %d, going to %s\n", t, #lab)

int main(int argc, const char **argv) {
  int t = 0;

  {
UP:
    if (t == 2) {
      describe(RET);
      goto RET;
    }
  }

AGAIN:
  {
    int a;
    a = 1;
    /*@assert \valid(&a); */

    if (t == 2) {
      describe(UP);
      /* When jumping to UP label we need to make sure that the
         program transformation does not insert a call deleting [b]. */
      goto UP;
    } else
      t++;

    int b = 15;
    /*@assert \valid(&b); */
    describe(AGAIN);
    goto AGAIN;
  }

RET:
  return 0;
}
