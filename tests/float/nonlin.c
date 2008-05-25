/* run.config
   OPT: -memory-footprint 1 -slevel 30 -val -cpp-command "gcc -C -E -I. " share/builtin.c 
*/

#include "share/builtin.h"

float a, b, c, r1, r2;

void main()
{
  a = Frama_C_float_interval(5.0, 7.0);
  b = Frama_C_float_interval(0.0, 1.0);
  c = 7.0;
  r1 = a + (b * (c - a));
  /*@ assert 
       (5.0 <= a <= 5.2) 
    || (5.2 <= a <= 5.4) 
    || (5.4 <= a <= 5.6) 
    || (5.6 <= a <= 5.8) 
    || (5.8 <= a <= 6.0) 
    || (6.0 <= a <= 6.2) 
    || (6.2 <= a <= 6.4) 
    || (6.4 <= a <= 6.6) 
    || (6.6 <= a <= 6.8) 
    || (6.8 <= a <= 7.0) ; */

  r2 = a + (b * (c - a));
  Frama_C_show_each_a_r2("a", a, "r2", r2);
}
