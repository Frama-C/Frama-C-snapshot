void main(void) {
  int i,n,x;
  for(i = 0; i < 10; i++) { n = i; }
  /*@ assert 0 <= n <= 9; */
  // only to show that n may be uninitialized at this point
  if (n) x = 0; else x = 1;
}
