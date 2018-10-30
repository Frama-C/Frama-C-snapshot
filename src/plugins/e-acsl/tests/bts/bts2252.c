/* run.config
   COMMENT: bts #2252, failures due to typing of offsets
*/

#include <stdlib.h>

int main() {
  char* srcbuf = "Test Code";
  int i, loc = 1;

  char * destbuf = (char*)malloc(10*sizeof(char));
  char ch = 'o';

  if (destbuf != NULL) {
    for (i = -1; i < 0; i++) {
      /*@ assert ! \valid_read(srcbuf + i); */
      if (srcbuf[i] == ch) { /* ERROR: Buffer Underrun */
        loc = i;
      }
    }

    strncpy (&destbuf[loc], &srcbuf[loc], 1);
    free(destbuf);
  }
}
