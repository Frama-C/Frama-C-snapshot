#include "../../share/libc/stdio.h"

void main(int x, ...) {
  int x,y;
  va_list p;
  va_start(p,x);
  vscanf("FOO %d %d",p);
}
