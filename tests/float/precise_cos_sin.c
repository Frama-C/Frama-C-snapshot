/* run.config*
   OPT: -eva @EVA_CONFIG@ -slevel 1000 -journal-disable -float-normal
*/

#include <__fc_builtin.h>
#include <math.h>


float Frama_C_float_interval(float, float);

int main(){
  float f = Frama_C_float_interval(-3.1875, -3.1875+0.25);  
  while (f <= 3.1875)
    { //Frama_C_show_each_f(f);
      Frama_C_show_each_s((float)sin(f));
      Frama_C_show_each_c((float)cos(f));
      f += 0.25;
    }
  return 0;
}
