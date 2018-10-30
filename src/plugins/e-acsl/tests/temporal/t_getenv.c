/* run.config
   COMMENT: Check temporal validity of environment string (via getenv function)
   STDOPT: #"-eva-warn-key=libc:unsupported-spec=inactive"
*/
#include <stdlib.h>
#include <errno.h>


int main(int argc, const char **argv) {
  char *g1 = NULL;
  g1 = getenv("HOME");
  char *g2 = getenv("PATH");

  /*@assert g1 == \null || \valid(g1); */
  /*@assert g2 == \null || \valid(g2); */
  return 0;
}
