/* run.config*
   OPT: -eva @EVA_CONFIG@ -journal-disable -eva-builtins-auto -calldeps
*/

// This test verifies that writing in a memory location that may be const
// is correctly handled

#include "string.h"

volatile int v;

const int a = -1;
int b;

void main0() {
  int *p = v ? &a : &b;
  int x = 1;
  memcpy(p, &x, sizeof(x));
  Frama_C_dump_each();
}

void main1() {
  int *p = v ? &a : &b;
  int x = 1;
  memset(p, 5, sizeof(x));
  Frama_C_dump_each();
}


void main2() {
  int *p = v ? &a : &b;
  *p = 1;
  Frama_C_dump_each();
}

void main3() {
  int *p = v ? &a : &b;
  int y = 2;
  *p = y;
  Frama_C_dump_each();
}

void main() {
  main0();
  main1();
  main2();
  main3();
}
