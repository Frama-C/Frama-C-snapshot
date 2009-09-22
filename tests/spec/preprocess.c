/* run.config
   OPT: -pp-annot -val -journal-disable
*/

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
  return 0;
}
