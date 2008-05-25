/* run.config
   OPT: -pp-annot -val
*/

int x = 1;

#include "preprocess.h"
int y = 1;
/*@ requires x >= MIN_X;
  behavior default:
    ensures test(\result);
*/

int f(int x) { return (x + MIN_X); }

int main() {
  int y = f(MIN_X);
  return 0;
}
