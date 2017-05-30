/* run.config
   STDOPT: #"-metrics-no-libc"
   STDOPT: #"-metrics-libc"
*/
#include <ctype.h>
#include <stdio.h> // defines external variables

int main() {
  return isblank(0);
}
