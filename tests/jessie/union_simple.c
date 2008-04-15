
typedef union {
  int i;
  char c;
} U;

void f(U *x) {
  x->i = 1;
  //@ assert x->i == 1;
  x->c = 1;
  //@ assert x->c == 1;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make union_simple"
End:
*/
