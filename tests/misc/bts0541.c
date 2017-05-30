/* run.config
   OPT: -pp-annot -cpp-extra-args="-I./share/libc" -pp-annot -val -val-show-progress
*/

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int main() {

  _Bool x = true;
  /*@ assert x==false ==> \false; */
  return 0;

}
