#include "share/libc/stddef.h"

struct c {char ca;};
void main(void) {
  size_t S;;
  S =  offsetof(struct c, ca);
  return;
}
