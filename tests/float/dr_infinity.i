/* run.config
   STDOPT: +"-warn-decimal-float all" +"-float-hex"
*/

void main(void)
{
	float x, y;
	x = 3.4028235677973366e+38f;
        Frama_C_show_each(x);
	y = (float) 3.402823567797366e+38;
}
