
#pragma SeparationPolicy(Regions)

/*@ requires \valid(x) && \valid(y);
  @ ensures *x == 0 && *y == \old(*y);
  @*/
void f(int *x, int *y) {
  *x = 0;
}

/*@ behavior values:
  @   ensures \true;
  @*/
void g() {
  int x = 1, y = 1;
  f(&x,&y);
  //@ for values: assert x == 0 && y == 1;

  int a[2] = { 1, 1 };
  f(a,a+1);
  //@ for values: assert a[0] == 0 && a[1] == 1;
}

/*@ requires \valid(x) && \valid(y);
  @ ensures *x == 0 && *y == 1;
  @*/
void h(int *x, int *y) {
  *x = 0;
  *y = 1;
}

/*@ behavior values:
  @   ensures \true;
  @*/
void k() {
  int x = 1, y = 2;
  h(&x,&y);
  //@ for values: assert x == 0 && y == 1;

  int a[2] = { 1, 2 };
  h(a,a+1);
  //@ for values: assert a[0] == 0 && a[1] == 1;
}


/*
Local Variables:
compile-command: "LC_ALL=C make -j separation"
End:
*/
