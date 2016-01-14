/* run.config
   OPT: -wp-model Caveat
*/

/* run.config_qualif
   OPT: -wp-model Caveat
*/

typedef struct S { int n ; int a[4]; } ;

/*@ 
  requires 0 <= n < 4 ;
  ensures N: p->n == n ;
  ensures A: \forall integer k; 0 <= k < n ==> p->a[k] == b[k];
  assigns *p ;
 */
void job( struct S * p , int n , int *b )
{
  p->n = n ;
  /*@ 
    loop invariant 0 <= i <= n ;
    loop invariant \forall integer k; 0 <= k < i ==> p->a[k] == b[k];
    loop assigns i, p->a[..];
  */
  for (int i = 0; i < n; i++) p->a[i] = b[i];
}
