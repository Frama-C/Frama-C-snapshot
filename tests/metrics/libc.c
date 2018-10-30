/* run.config
   STDOPT: #"-metrics-no-libc -metrics-value-cover"
   STDOPT: #"-metrics-libc -metrics-value-cover"
*/
#include <ctype.h>
#include <stdio.h> // defines external variables
#include <getopt.h>

// getopt will have the fc_stdlib attribute, but foo and bar won't;
// ensure they are not skipped during syntactic search

int foo() { return 42; }

int bar() { return 42; }

int f() { // never called
  return getchar();
}

int g() { // called via fp
  return isalpha(42);
}

int (*fp)() = g;

int getopt(int argc, char * const argv[],
           const char *optstring) {
  return foo() + bar();
}

int main() {
  fp();
  getopt(0, 0, 0);
  return isblank(0);
}
