/*run.config
  STDOPT: +"-warn-special-float none"
 */

#include <assert.h>

void (*fp)(int);
volatile double naan;
struct s { int a; } s1;

volatile int nondet;
void main() {
  if (nondet) assert(nondet); // unknown
  if (nondet) assert(0); // invalid
  if (nondet) assert(1); // valid
  if (nondet) assert(fp); // invalid
  if (nondet) assert(naan); // unknown
}
