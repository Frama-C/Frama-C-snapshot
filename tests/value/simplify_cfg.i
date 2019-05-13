/* run.config*
   OPT: -no-autoload-plugins -load-module eva,inout -simplify-cfg -keep-switch -eva @EVA_CONFIG@ -journal-disable
   OPT: -no-autoload-plugins -load-module eva,inout -simplify-cfg -eva @EVA_CONFIG@ -journal-disable
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
