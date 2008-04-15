/*
@PPC_OPTIONS: -jessie-std-stubs
*/

#include <stdlib.h>
#include <string.h>

void f(char *FRAMA_C_STRING x) {
  size_t len = strlen(x);
  char *y = calloc(len+1,sizeof(char));
  memcpy(y,x,len);
  //@ assert valid_string(y);
}

/* 
Local Variables:
compile-command: "LC_ALL=C make calloc"
End:
*/
