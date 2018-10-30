/* run.config
   COMMENT: Check whether location of errno is recorded
*/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, const char **argv) {
  int *p = &errno;
  /*@ assert \valid(p); */
  return 0;
}
