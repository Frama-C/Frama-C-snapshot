/* run.config
   OPT: -rte -rte-float-to-int -print -machdep x86_32 -journal-disable
*/

void main() {
  float f = 0.;

  int i = f;
  long long l = f;
  unsigned short s = f;

  int ci1 = 1.5;
  int ci2 = 1.5e255;
  char ci3 = 258.;
  int ci4 = 2147483647.5;
  int ci5 = -2147483649.5;

}
