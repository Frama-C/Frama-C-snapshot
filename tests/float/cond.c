/* run.config
   OPT: -memory-footprint 1 -val -cpp-command "gcc -C -E -I. " share/builtin.c -journal-disable -float-hex
*/

#include "share/builtin.h"

float x,y,z;
double dx,dy,dz;
void main(int c)
{
  x = Frama_C_float_interval(-10.0, 10.0);
  x = x >= 0.0 ? x : 0.0;
  dx = Frama_C_float_interval(-10.0, 10.0);
  dx = dx >= 0.0 ? dx : 0.0;
}
