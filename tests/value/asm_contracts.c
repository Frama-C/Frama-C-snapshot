


#include <stdint.h>

void f1() {
  int src = 1;
  int dst = 0;

  asm ("mov %1, %0\n\t"
       "add $1, %0"
       : "=r" (dst)
       : "r" (src));

  //@ assert OK: src == 1;
  //@ assert UNKNOWN1: dst != 0;
  //@ assert UNKNOWN2: dst == 2;
}

void f2() {
  uint32_t dwRes;
  uint32_t dwSomeValue = 42;
  asm ("bsfl %1,%0"
       : "=r" (dwRes)
       : "r" (dwSomeValue)
       : "cc"); // FLAGS register is clobbered
  //@ assert OK: dwSomeValue == 42;
  //@ assert UNKNOWN1: dwRes != 0;
}

void f3() {
  int x = 1;
  asm ("bla");
  //@ assert UNKNOWN: x == 1;
}

int main() {
  f1();
  f2();
  f3();
  return 0;
}
