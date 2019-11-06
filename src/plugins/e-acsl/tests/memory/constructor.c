/* run.config
   COMMENT: bts #2405. Memory not initialized for code executed before main.
*/

#include <stdio.h>
#include <stdlib.h>

__attribute__((constructor))
void f() {
  printf("f\n");
  char *buf = (char*)malloc(10*sizeof(char));
  free(buf);
}

int main() {
  printf("main\n");
  return 0;
}