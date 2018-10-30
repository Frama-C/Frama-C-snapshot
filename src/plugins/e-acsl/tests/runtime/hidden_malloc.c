/* run.config
   COMMENT: Malloc executed by a library function
*/

#include <limits.h>
#include <stdlib.h>

int main(int argc, const char **argv) {
  /* If the second argument of `realpath` is NULL it uses malloc.
     Make sure that memory layout has been initialized. */
  char *cwd = realpath(".", NULL);
  return 0;
}
