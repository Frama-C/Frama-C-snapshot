int simple_inv (void) {
  int i = 0;
  while (i < 5) {
    //@ invariant 0 <= i < 5 ;
    i++;
  }
  return i;
}
int inv_from_init (void) {
  int x = 5;
  int i = 0;
  while (i < 5) {
    //@ invariant i < x ;
    i++;
  }
  return i;
}
/*
int caveat_inv (int n) {
  int i, s = 0;
  //@ loop assigns i, s;
  for (i = 0; i < n; i++) {
    //@ invariant 0 <= i < n ;
    s++;
  }
  return s;
}
*/
int main (void) { return 0 ; }
