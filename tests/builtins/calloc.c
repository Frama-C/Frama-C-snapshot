/* run.config*
 STDOPT: #"-no-val-builtins-auto -val-alloc-returns-null"
 STDOPT: #"-no-val-builtins-auto -val-alloc-returns-null -val-builtin calloc:Frama_C_calloc_fresh"
 STDOPT: #"-no-val-builtins-auto -val-alloc-returns-null -val-builtin calloc:Frama_C_calloc_by_stack"
 STDOPT: #"-no-val-builtins-auto -no-val-alloc-returns-null -val-builtin calloc:Frama_C_calloc_fresh"
 STDOPT: #"-no-val-builtins-auto -no-val-alloc-returns-null -val-builtin calloc:Frama_C_calloc_by_stack"
 */

#include <stdlib.h>
#include <stdint.h>

volatile int nondet;
int main() {
  char *p1 = calloc(0, 0);
  if (!p1) return 1;

  char *p2 = calloc(1, 0);
  if (!p2) return 1;

  char *p3 = calloc(0, 1);
  if (!p3) return 1;

  char *p4 = calloc(1, 1);
  if (!p4) return 1;
  if (nondet) {
    //@ assert \valid(p4); // fails when builtin not used
    //@ assert *p4 == 0;
  }

  int *p5 = calloc(1024, sizeof(int));
  if (!p5) return 1;
  if (nondet) {
    //@ assert \valid(p5+(0..1023)); // fails when builtin not used
    //@ assert *p5 == 0;
    //@ assert p5[1023] == 0;
  }

  char *p9001 = calloc(SIZE_MAX - 1, 2);
  if (p9001) return 1;
  //@ assert p9001 == \null;

  return 0;
}
