/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
   OPT: -wp-model +ref
*/

//@ predicate vrange(int *p1,integer n) = \valid(p1+(0..n-1)) ;

//@ requires vrange(a,k) && 0<=i<k ; assigns a[i];ensures a[i] == 0 ;
void fvrange_n (int *a,int i,int k)
{
  //@ assert \valid(a+i);
  a[i] = 0 ;
}



//@ predicate P(integer b) = b == 0  ;
 

//@   requires P(x) ; assigns \nothing; ensures P(\result) ;
int gcd(int x) {return x;}
