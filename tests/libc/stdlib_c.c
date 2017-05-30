/* run.config
   STDOPT: #"-no-val-builtins-auto -slevel 10 -val-builtin calloc:Frama_C_calloc_by_stack -value-msg-key malloc"
   STDOPT: #"-no-val-builtins-auto -slevel 10 -val-builtin calloc:Frama_C_calloc_by_stack -no-val-malloc-returns-null -value-msg-key malloc"
   STDOPT: #"-no-val-builtins-auto"
*/ // slevel is used to unroll loops

#define FRAMA_C_MALLOC_STACK
#include "stdlib.c"
#include "__fc_builtin.h"
#include <stdint.h>

int main() {
  // always succeeds if -no-val-malloc-returns-null, otherwise may succeed
  int *p = calloc(1, sizeof(int));
  if (p) {
    //@ assert \valid(p);
  }

  // partial overflow
  size_t nmemb = Frama_C_size_t_interval(1, SIZE_MAX);
  int *q = calloc(nmemb, sizeof(int));
  if (q) {
    //@ assert \valid(q);
  }

  // never succeeds (always overflows)
  int *r = calloc(SIZE_MAX, sizeof(int));
  //@ assert !r;

  // may succeed for some cases, but fail later
  for (size_t i = 1; i < SIZE_MAX; i++) {
    int *s = calloc(i, sizeof(int));
    if (s) s[i-1] = 42;
  }

  return 0;
}
