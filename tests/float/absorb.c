/* run.config
   EXECNOW: BIN absorb.sav LOG absorb_sav.res LOG absorb_sav.err ./bin/toplevel.opt -memory-footprint 1 -val -journal-disable share/builtin.c -float-hex -save ./tests/float/result/absorb.sav tests/float/absorb.c > tests/float/result/absorb_sav.res 2> tests/float/result/absorb_sav.err
   OPT: -load ./tests/float/result/absorb.sav -deps -out -input
   OPT: -all-rounding-modes -memory-footprint 1 -val -deps -out -input -journal-disable -float-hex share/builtin.c
*/

#include "share/builtin.h"

float x = 1.0, y = 0.0, z, t;

void main() {
  long long b = Frama_C_interval(-2000000001, 2000000001);
  b = b * b;
  z = y + 1e-286;
  while (y != x)
  {  
    y = x ; x+=1E-286;
  }
  t = b;
}
