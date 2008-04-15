
/*@ axiomatic P { 
  @  predicate p{L}(int *x);
  @   // reads *x;
  @ axiom p_footprint{L} : \forall int* x; p(x) && *x ==> p(x) && *x;
  @ }
  @*/

/*@ requires \valid_range(x,0,1) && p(x);
  @ assigns *(x+1);
  @*/
void f(int *x) {
  *(x+1) = 0;
  //@ assert p(x);
}

/*@ requires \valid_range(x,0,1) && p(x);
  @ assigns *(x+1);
  @*/
void g(int *x) {
  f(x);
  //@ assert p(x);
}

/*
Local Variables:
compile-command: "LC_ALL=C make footprint"
End:
*/
