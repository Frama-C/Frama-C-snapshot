/* run.config
   COMMENT: Off-by-one error where a pointer is made point to an adjacent block
   COMMENT: Note that this behaviour is not quaranteed by likely due to the
   COMMENT: way compiler allocates stack blocks
*/

#include <stdint.h>

#define intaddr(_x) ((uintptr_t)&_x)

int main(void) {
  int i = 0;
  /* Assuming here that arr is allocated first, and arr2 is allocated second
     and that there is no gap betwee [arr] and [arr2]. This behaviour is
     compiler specific but still likely. At least this is the case for some
     current versions of GCC/Clang */
  int arr  [] = {1, 2, 3, 4};
  int arr2 [] = {1, 2, 3, 4};

  /* Check that the above condition holds, otherwise there is no point
     having this test */
  if (!(intaddr(arr) + sizeof(arr) == intaddr(arr2)))
    return 0;

  int *q = arr;

  while (i < 4) {
    /*@assert \valid(q); */
    *q = 1; /* This dereference of [q] is valid but the last [q++]  */
    i++;    /* makes [q] point to [arr2] */
    q++;
  }

  /* At this point the dereference is temporally invalid since [q] should store
     the referent number of [arr1] while in fact it points to [arr2] */
  /*@assert ! \valid(q); */
  *q = 1;
  return 0;
}
