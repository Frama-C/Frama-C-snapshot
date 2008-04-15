/* run.config
   DONTRUN: union no working yet
*/

typedef union {
  int i;
  char c;
  int *p;
} U;

/*@ requires \valid(x) && \valid(x->p) && x->c == 0;
  @ ensures x->i == 0 && x->c == 0 && \result == 0;
  @ */
int f(U* x) {
  *x->p = 0;
  //@ assert *x->p == 0;
  x->i = 0;
  return x->i;
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j discr_union"
End:
*/
