/* run.config
   STDOPT:
*/

#include <stdio.h>

// <stdlib.h> contains this function, but we did not include it, so it should
// not be reported as part of the stdlib.
int rand(void);

void my_printf(char const *s) {}

void printf2(char *s);

void main() {
  printf("this call is part of the stdlib");
  my_printf("this call is not part of the stdlib");
  rand();
}
