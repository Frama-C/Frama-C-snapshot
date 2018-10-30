/* run.config
   COMMENT: Simple case of double pointer dereference
*/

#include <stdlib.h>

int main(void) {
  int **p = malloc(sizeof(int*)*3);
  int i = 0;

  while (i < 3) {
    /*@assert \valid(p+i); */
    *(p+i) = malloc(sizeof(int));
    /*@assert \valid(*(p+i)); */
    i++;
  }

  free(*(p+2));
  malloc(sizeof(int));
  /*@assert ! \valid(*(p+2)); */
  return 0;
}
