/* run.config
  STDOPT: +"-warn-decimal-float all"
  STDOPT: +"-warn-decimal-float all -all-rounding-modes-constants -float-hex"
*/

double f1 = 1e-40f;
double d0 = 1e-40;

int main()
{
  Frama_C_dump_each();
  double d1 = f1;
}
