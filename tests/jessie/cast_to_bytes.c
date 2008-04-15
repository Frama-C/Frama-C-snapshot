/* run.config
   DONTRUN: cast no working yet
*/
#pragma IntModel(modulo)

#include <limits.h>

//@ requires 0 < size < 10 && \valid_range((char*)p,0,4*size-1);
void f(int* p, int size) {
  char* c = (char*) p;
  int i = 0;
  //@ loop invariant i >= 0;
  while (i < size * sizeof(int)) {
    c[i] = 0;
    ++i;
  }
}

//@ requires \valid(p) && \valid(q);
void g(int* p, int* q) {
  char* c = (char*) p;
  char* d = (char*) q;
  c[0] = d[0];
  c[1] = d[1];
  c[2] = d[2];
  c[3] = d[3];
  //@ assert (*c == *d);
}

/* 
Local Variables:
compile-command: "LC_ALL=C make cast_to_bytes"
End:
*/
