
//@ ensures \result == 0;
int f() {
  return 0;
}

/*@ behavior result_ge_x:
  @   ensures \result >= x;
  @ behavior result_ge_y:
  @   ensures \result >= y;
  @ behavior result_is_lub:
  @   ensures \forall int z; z >= x && z >= y ==> z >= \result;
  @*/
int max(int x, int y) {
  return (x>y) ? x : y;
}

//@ ensures *p >= 0;
void abs1(int *p) {
  if (*p < 0) *p = 0-*p;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make basic"
End:
*/
