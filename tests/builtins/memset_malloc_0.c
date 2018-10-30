/* run.config*

*/



#include <stdlib.h>



#include <string.h>

long *p;

int main(){
  long l;
  p = malloc(0);
  memset(p, 0, 0); // succeeds if p != NULL (implementation-defined)
}
