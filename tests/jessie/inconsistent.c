
//@ requires \valid(p) && ! \valid(q);
void f(int *p, int *q) {
  if (p == q) {
    //@ assert 0 == 1;
  }
}
