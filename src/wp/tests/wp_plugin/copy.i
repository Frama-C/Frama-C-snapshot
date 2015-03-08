// Everything should be discharged.

/*@ requires n>=0 ;
  @ requires \separated( a+ (0..n-1) , b + (0..n-1) );
  @ ensures \forall integer k ; 0 <= k < n ==> a[k] == b[k] ;
  @ assigns a[0..n-1] ;
  @ */
void copy( int * a , int * b , int n )
{
  /*@ loop invariant Range: 0 <= i <= n ;
    @ loop invariant Copy:  \forall integer k ; 0 <= k < i ==> a[k] == b[k] ;
    @ loop assigns i , a[0..n-1] ;
    @ */
  for (int i = 0 ; i < n ; i++) {
  L:
    a[i] = b[i] ;
    /*@ assert A: \forall integer k ; 0 <= k < i ==> a[k] == \at(a[k],L); */
    /*@ assert B: \forall integer k ; 0 <= k < i ==> b[k] == \at(b[k],L); */
  }
}
