/* run.config
   OPT: -rte -warn-not-finite-float -print -machdep x86_32 -journal-disable
*/
#define _ISOC99_SOURCE
#include <math.h>

void main() {
  double d = 0x1p10000;
  d = 0.;
  double e = (d/d) + d;
}
