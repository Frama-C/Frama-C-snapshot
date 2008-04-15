//@ requires \base_addr(p1)!=\base_addr(p2);
char f(int* p1, int *p2) {
  //@ assert (p1 != p2);
  return 0;
}

