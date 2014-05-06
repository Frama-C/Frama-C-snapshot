/* run.config
   EXECNOW: BIN bool.sav LOG bool_sav.res LOG bool_sav.err ./bin/toplevel.opt -save ./tests/saveload/result/bool.sav -val ./tests/saveload/bool.c > tests/saveload/result/bool_sav.res 2> tests/saveload/result/bool_sav.err
   OPT: -load ./tests/saveload/result/bool.sav -out -input -deps
   OPT: -load ./tests/saveload/result/bool.sav -val
 */

#include "stdbool.h"
#include "share/libc/stdio.h"

bool x;
int y;

int f() {
  int i, j;

  i = 10;
  /*@ assert (i == 10); */
  while(i--);
  j = 5;

  return 0;
}

int main() {
  f();
  x=false;
  printf("%d\n",x);
  x=2;
  printf("%d\n",x);
  y=x+1;
  printf("%d,%d\n",x,y);
  x=x+1;
  printf("%d\n",x);
  x=x+1;
  printf("%d\n",x);
  return y;
}
