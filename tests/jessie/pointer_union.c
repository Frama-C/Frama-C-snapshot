
union U {
  int i;
  int* p;
};

//@ requires \valid(x);
void zero(union U* x) {
  x->i = 0;
  //@ assert x->i == 0;
  x->p = (int*)malloc(sizeof(int));
  *x->p = 1;
  //@ assert *x->p == 1;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make pointer_union"
End:
*/
