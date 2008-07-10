/* run.config
   DONTRUN: support for discussion on atexit specification
*/
/* Asked question:
 * How to specify this code with ACSL ?
 */

#ifndef PTEST
#include <stdio.h>
#else
extern int printf (__const char *__restrict __format, ...);
#endif

#ifndef PTEST
#include <stdlib.h>
#else
extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
#endif

char *glob;
int res;

void test(void) {
  printf("%s (%d);\n", glob, res);
}

int main(int argc, char *argv[]) {
  atexit(test);
  res = argc - 2 ;
  if (res > 1) {
    glob = "exit";
    exit (res);
    }
  glob = "return";
  return res;
}
