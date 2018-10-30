/* run.config
  COMMENT: Case when a pointer is taking address by value.
*/

#include <stdint.h>

int main (int argc, char **argv) {
  uintptr_t addr = (uintptr_t)&argc;
  char *p;
  int *q;
  q = &argc;
  /* Here the referent of p should be assigned from the value of addr */
  p = (char*)addr;
  p = (char*)0x123456;
  p = (char*)q;
}


