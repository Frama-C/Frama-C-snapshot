/* run.config_qualif
   OPT: -wp -wp-par 1
*/

/*@ predicate IsEqual(int * a , int * b , int n) =
  @   \forall integer i; 0 <= i < n ==> a[i] == b[i] ;
  @   */

/*@ requires n>0 ;
  @ requires \valid( a+ (0..n-1) );
  @ requires \valid( b+ (0..n-1) );
  @ requires \separated( a+ (0..n-1) , b + (0..n-1) );
  @ ensures IsEqual(a,b,n);
  @ assigns b[0..n-1] ;
  @ */
void copy( int * a , int * b , int n )
{
  /*@ loop invariant 0 <= i <= n ;
    @ loop invariant IsEqual(a,b,i) ;
    @ loop assigns i , b[0..n-1] ;
    @ */
  for (int i = 0 ; i < n ; i++) {
    b[i] = a[i] ;
  }
}
