
/*@ requires n >= 0 && \valid_range(t,0,n-1);
  @ assigns t[0..n-1];
  @ ensures \forall int k; 0 <= k < n ==> t[k] == -\old(t[k]);
  @*/
void negate(int *t, int n) {
  int i = 0;
  /*@ loop invariant 
    @   0 <= i <= n && 
    @   (\forall int k; 0 <= k < i ==> t[k] == -\at(t[k],Pre)) ;
    @ loop assigns t[0..i-1];
    @ loop variant n-i; */
  while (i < n) {
    t[i] = -t[i];
    i++;
  }
}


/* 
Local Variables:
compile-command: "LC_ALL=C make negate"
End:
*/
