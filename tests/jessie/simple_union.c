
union U {
  int i;
  struct { short s1; short s2; } s;
};

//@ requires \valid(x);
void zero(union U* x) {
  x->i = 0;
  //@ assert x->s.s1 == 0;
  //@ assert x->s.s2 == 0;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make simple_union"
End:
*/
