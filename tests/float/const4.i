/* run.config
  STDOPT: +"-warn-decimal-float all"
  STDOPT: +"-warn-decimal-float all -all-rounding-modes-constants"
*/

double f1 = 3.4e38f;
double f2 = 3.405e38f;

int main()
{
  Frama_C_dump_each();
  double d2 = f2;
}
