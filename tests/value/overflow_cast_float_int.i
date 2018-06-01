/* run.config*

*/

volatile v;

int main()
{
  float vf1;
  signed int e;
  unsigned int d;
  int c1, c2;
  d = 0x7FFFFFFFll;
  if (v) {
    vf1 = d * 1.0;
    e = (int)vf1;
  }
  c1 = 2147483647.5;
  if (v) {
    c2 = -2147483649.5;
  }
  double dd = v ? -0x1.967ae928d56b0p66 : -0x1.c5e2546cfeb1ap34;
  if (v) {
    double k = (int)dd;
  }

}

