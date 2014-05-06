/* run.config
   OPT: -val -cpp-command "gcc -C -E -I. " share/builtin.c -journal-disable -float-hex
*/

#include "share/builtin.h"

float x, y, z;
double dx, dy, dz, dt;
int c1,c2;
void main(int c)
{
  x = Frama_C_float_interval(-10.0, 10.0);
  x = x >= 0.0 ? x : 0.0;

  dx = Frama_C_float_interval(-10.0, 10.0);
  dx = dx >= 0.0 ? dx : 0.0;
  c1 = dx >= 0;
  /*@ assert ! (0. <= dx <= 1.) ; */

  dz = Frama_C_float_interval(-10.0, 10.0);
  dt = dz < 1.0 ? dz : 0.0;
  dz = dz > 1.0 ? dz : 2.0;

}
