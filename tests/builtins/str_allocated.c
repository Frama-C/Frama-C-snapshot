/* run.config*

*/
#include <string.h>
#include <stdlib.h>
volatile int nondet;

void memchr_bug() {
  char *b;
  int i;
  for (i = 0; i < 2; i++) {
    b = malloc(i);
  }
  memchr(b, 1, 1); // Results in Bottom, but should not result in
                   // degeneration of the analysis
}

int main() {
  if (nondet) memchr_bug();
  return 0;
}
