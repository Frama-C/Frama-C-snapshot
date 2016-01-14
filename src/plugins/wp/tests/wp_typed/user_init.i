/*@ requires \valid(a+(0..n-1)) ;
  @ requires n >= 0 ;
  @ ensures \forall int k ; 0 <= k < n ==> a[k] == v ;
  @ assigns a[0..n-1] ;
*/
void init( int * a , int n , int v )
{
  /*@ loop invariant Range: 0 <= i <= n ;
    @ loop invariant Partial: \forall int k ; 0 <= k < i ==> a[k] == v ;
    @ loop assigns i,a[0..n-1] ;
  */
  for (int i = 0 ; i < n ; i++) a[i] = v ;
}
