
//@ requires 0 <= n; // notice that we should be able to relax that.
int loop_var (int n) {
  int i, s = 0;
  /*@ 
    loop assigns i, s;
    loop variant (n - i);
  */
  for (i = 0; i < n; i++) {
    s++;
  }
  return s;
}
int main (void) { return 0 ; }
