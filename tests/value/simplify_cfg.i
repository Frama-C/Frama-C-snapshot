/* run.config*
   OPT: -no-autoload-plugins -load-module value,inout -simplify-cfg -keep-switch -val @VALUECONFIG@ -journal-disable
   OPT: -no-autoload-plugins -load-module value,inout -simplify-cfg -val @VALUECONFIG@ -journal-disable
*/

int main(int x, int y) {
  int z = 0;
  char c = 'c';
  switch (x) {
  case 0: z=(int)c;
  default: z++;
  }
  return z;
}
