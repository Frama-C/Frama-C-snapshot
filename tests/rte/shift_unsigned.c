/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32
   OPT: -rte -warn-signed-overflow -warn-unsigned-overflow -print -machdep x86_32
*/

int main () {
  unsigned int x, y;
  x= 0x10000000U;
  y = x << 4;
  y = 0x10000000U << 4;
  y = 1U << -3;
  y = -4 << 2;
  return y;
}
