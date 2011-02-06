/* run.config 
   DONTRUN: test under construction
*/

//@ ensures \result >= n;
int loop_invariant(int n) {
  int i = 0 ;
  //@ loop invariant n== \at(n,Pre) && 0 <= i && (0 <=n ==> i <= n);
  while (i < n)
    i++ ;
  return i;
}


//@ ensures \result >= n;
int invariant_as_loop_invariant(int n) {
  int i = 0 ;
  while (i < n)
    //@ invariant n== \at(n,Pre) && 0 <= i && i <= n;
    i++ ;
  return i;
}
