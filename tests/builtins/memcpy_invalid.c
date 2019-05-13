/* run.config*
   OPT: -eva @EVA_CONFIG@ -journal-disable -calldeps
*/

/*@ assigns \result \from min, max;
  @ ensures min <= \result <= max;
*/
extern int Frama_C_interval(int min, int max);

#include "string.h"
volatile int v;

static void test(int max)
{
        char dst, src = 0;
        unsigned long i = Frama_C_interval(0, max) * 2U;
        memcpy(&dst, (char *)&src + i, sizeof(src) - i);
}

extern int main(void)
{
  if (v) test(7);
  if (v) test(8);
  return 0;
}

