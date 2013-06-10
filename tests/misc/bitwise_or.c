/* run.config
   OPT: -big-ints-hex 256 -val -journal-disable share/builtin.c 
*/
#include "../../share/builtin.h"

int or1, or2, or3, or4, or5;
int and1, and2, and3, and4, xor1, xor2;
unsigned int uand1, uand2, uand3, uand4, uand5;
int a,b,c,d,e;
extern unsigned short s;

main(){
  a = Frama_C_interval(3,17);
  b = Frama_C_interval(-3,17);
  c = Frama_C_interval(13,27);
  or1 = a | b;
  or2 = a | c;
  or3 = b | c;

  and1 = a & b;
  and2 = a & c;
  and3 = b & c;

  uand4 = 0xFFFFFFF8U & (unsigned int) c;

  xor1 = a ^ a;
  xor2 = a ^ b;

  unsigned i1 = s * 2;
  unsigned i2 = s * 4;
  unsigned v1 = i1 & i2;
  unsigned v2 = i1 | i2;

  return 0;
}
