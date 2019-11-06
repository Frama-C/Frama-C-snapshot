/* run.config
  COMMENT: Check if the instrumentation engine still adds __e_acsl_memory init
  COMMENT: is inserted for the case when no malloc is used but no no variable
  COMMENT: is required tracking
*/

#include <stdlib.h>

int main(void) {
    /* @assert (__heap_allocation_size == 0);  */
    char *a = malloc(7);
    /* @assert (__heap_allocation_size == 7);  */
}
