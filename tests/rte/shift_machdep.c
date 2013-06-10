/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32
   OPT: -rte -warn-signed-overflow -print -machdep x86_64
*/


int main() {
  int x; long y;
  x = 5 << 30;
  y = 5L << 30;
  return 0;
}
