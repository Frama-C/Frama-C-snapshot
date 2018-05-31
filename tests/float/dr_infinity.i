/* run.config*
   STDOPT: #"-warn-decimal-float all -float-hex"
*/
volatile v;
void main(void)
{
        float x; double d;
	x = 3.4028235677973366e+38f;
        Frama_C_show_each(x);
        if (v) {
          x = (float) 3.402823567797366e+38; // double constant, that overflows the valid range for float32
          Frama_C_show_each_dead_x_1(x); // dead;
        }
        if (v) {
          x = 3.4e38;
          x = x * x;
          Frama_C_show_each_dead_x_2(x); // dead;
        }
        if (v) {
          d = 1e308;
          d = d * 10;
          Frama_C_show_each_dead_d_1(d); // dead;
        }
        if (v) {
          d = -0.;
          d = 2/d;
          Frama_C_show_each_dead_d_2(d); // dead;
        }
        if (v) {
          d = 1e308;
          d = d / 0.01;
          Frama_C_show_each_dead_d_3(d); // dead;
        }
        if (v) {
          x = 1e38;
          x = x / 0.001;
          Frama_C_show_each_dead_x_3(x); // dead;
        }
        if (v) {
          d = 0;
          d = d / 0. ;
          Frama_C_show_each_dead_d_4(x); // dead;
        }
}
