
//@ predicate val{P,Q}(int *x) = \at(\valid(x),P);

//@ requires \valid(x);// && (char*)x == (char*)\base_addr(x);
void f(int *x) {
  free(x);
  //@ assert val{Pre,Here}(x);
}

int g;

//@ predicate valg{P,Q} = \at(g == 0,P);

//@ requires g == 0;
void h() {
  g = 1;
  //@ assert valg{Pre,Here};
}

/* 
Local Variables:
compile-command: "LC_ALL=C make -j labels"
End:
*/
