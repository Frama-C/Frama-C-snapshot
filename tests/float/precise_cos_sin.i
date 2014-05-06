/* run.config
   OPT: -val -obviously-terminates -journal-disable -float-normal share/builtin.c
*/

double Frama_C_cos_precise(double);
double Frama_C_sin_precise(double);
float Frama_C_float_interval(float, float);

main(){
  float f = Frama_C_float_interval(-3.1875, -3.1875+0.25);  
  while (f <= 3.1875)
    {
      Frama_C_show_each_s((float)Frama_C_sin_precise(f));
      Frama_C_show_each_c((float)Frama_C_cos_precise(f));
      f += 0.25;
    }
}
  
