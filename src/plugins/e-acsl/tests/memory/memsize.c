/* run.config
   COMMENT: Checking heap memory size
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

extern size_t __e_acsl_heap_allocation_size;

int main(int argc, char **argv) {
    /* Allocation increases */
    char *a = malloc(7);
    /*@assert (__e_acsl_heap_allocation_size == 7);  */
    char *b = malloc(14);
    /*@assert (__e_acsl_heap_allocation_size == 21);  */

    /* Allocation decreases */
    free(a);
    /*@assert (__e_acsl_heap_allocation_size == 14);  */

    /* Make sure that free with NULL behaves and does not affect allocation */
    a = NULL;
    free(a);
    /*@assert (__e_acsl_heap_allocation_size == 14);  */

    /* Realloc decreases allocation */
    b = realloc(b, 9);
    /*@assert (__e_acsl_heap_allocation_size == 9);  */

    /* Realloc increases allocation */
    b = realloc(b, 18);
    /*@assert (__e_acsl_heap_allocation_size == 18);  */

    /* Realloc with 0 is equivalent to free */
    b = realloc(b, 0);
    b = NULL;
    /*@assert (__e_acsl_heap_allocation_size == 0);  */

    /* realloc with 0 is equivalent to malloc */
    b = realloc(b, 8);
    /*@assert (__e_acsl_heap_allocation_size == 8);  */

    /* Abandon b and behave like malloc again */
    b = realloc(NULL, 8);
    /*@assert (__e_acsl_heap_allocation_size == 16);  */

    /* Make realloc fail by supplying a huge number */
    b = realloc(NULL, SIZE_MAX);
    /*@assert (__e_acsl_heap_allocation_size == 16);  */
    /*@assert (b == NULL);  */

    /* Same as test for calloc ... */
    b = calloc(SIZE_MAX, SIZE_MAX);
    /*@assert (__e_acsl_heap_allocation_size == 16);  */
    /*@assert (b == NULL);  */

    /* ... and for malloc */
    b = malloc(SIZE_MAX);
    /*@assert (__e_acsl_heap_allocation_size == 16);  */
    /*@assert (b == NULL);  */
    return 0;
}
