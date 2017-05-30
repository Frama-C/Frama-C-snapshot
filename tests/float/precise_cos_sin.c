/* run.config*
   OPT: -val @VALUECONFIG@ -slevel 1000 -journal-disable -float-normal
*/

#include <__fc_builtin.h>

double Frama_C_cos(double);
double Frama_C_sin(double);
float Frama_C_float_interval(float, float);

main(){
  float f = Frama_C_float_interval(-3.1875, -3.1875+0.25);  
  while (f <= 3.1875)
    {
      Frama_C_show_each_s((float)Frama_C_sin(f));
      Frama_C_show_each_c((float)Frama_C_cos(f));
      f += 0.25;
    }
}
  
