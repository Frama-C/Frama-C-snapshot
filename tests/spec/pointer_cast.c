void f(int **a) {
  int *b;
  //@ assert a == b;
  //@ assert (int*)a == b;
  //@ assert a == (int**)b;
}
