/* run.config
   OPT: -rte -rte-print -machdep x86_32 -journal-disable
   OPT: -rte -rte-unsigned-ov -rte-print -machdep x86_32 -journal-disable 
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
