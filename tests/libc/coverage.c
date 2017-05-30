/* run.config*
   OPT: -no-val-builtins-auto -val-show-progress share/libc/string.c -val -slevel 6 -metrics-value-cover -then -metrics-libc
*/

#include "string.h"

void main() {
  char *s = "blabli";
  int l = strlen(s);
}
