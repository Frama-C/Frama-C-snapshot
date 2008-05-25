/* run.config
   OPT: -simplify-cfg -keep-switch -val -files-debug -check
   OPT: -simplify-cfg -val -files-debug -check
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
