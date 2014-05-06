/* run.config
   EXECNOW: BIN absorb.sav LOG absorb_sav.res LOG absorb_sav.err FRAMAC_PLUGIN=tests/.empty @frama-c@ -journal-disable share/builtin.c -save @PTEST_DIR@/result/absorb.sav @PTEST_FILE@ > @PTEST_DIR@/result/absorb_sav.res 2> @PTEST_DIR@/result/absorb_sav.err
   EXECNOW: BIN absorb.sav2 LOG absorb_sav2.res LOG absorb_sav2.err FRAMAC_PLUGIN=tests/.empty @frama-c@ -load @PTEST_DIR@/result/absorb.sav -val -journal-disable -float-hex -save @PTEST_DIR@/result/absorb.sav2 > @PTEST_DIR@/result/absorb_sav2.res 2> @PTEST_DIR@/result/absorb_sav2.err
   OPT: -load @PTEST_DIR@/result/absorb.sav2 -deps -out -input
   OPT: -all-rounding-modes -val -deps -out -input -journal-disable -float-hex share/builtin.c
*/

#include "share/builtin.h"

float x = 1.0, y = 0.0, z, t, min_f, min_fl, den;

void main() {
  long long b = Frama_C_interval(-2000000001, 2000000001);
  b = b * b;
  z = y + 1e-286;
  while (y != x)
  {  
    y = x ; x+=1E-286;
  }
  t = b;
  min_f = 1.175494351e-38;
  min_fl = -1.1754943505e-38;
  den = min_f / 128.;
}
