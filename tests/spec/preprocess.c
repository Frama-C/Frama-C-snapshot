/* run.config
   OPT: -eva @EVA_CONFIG@ -journal-disable -print
*/

// see bts 1357
#define assert(x) (x)?1:0

int x = 1;

#define FOO 1
#undef FOO
#define FOO 2

#include "preprocess.h"
int y = 1;
/*@ requires x >= MIN_X;
  behavior default:
    ensures test(\result) && FOO == FOO;
*/

int f(int x) { return (x + MIN_X); }

int main() {
  int y = f(MIN_X);
  //@ assert (x) == 1;

  // BTS 2307
  /*@ assert backslash_string: "\\"[0] == '\\'; */
  return 0;
}
