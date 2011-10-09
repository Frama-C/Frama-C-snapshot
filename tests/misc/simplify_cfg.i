/* run.config
   OPT: -simplify-cfg -keep-switch -val -check -journal-disable
   OPT: -simplify-cfg -val -check -journal-disable
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
