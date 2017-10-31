/* run.config
   OPT: -wp-prenex
*/
/* run.config_qualif
   OPT: -wp-prenex
*/

/*@
  requires 0 <= n && 0 <= m;
  ensures
    \result <==>
    ( \forall integer i; 0 <= i < n ==>
      \forall integer j; 0 <= j < m ==>
      p[i] < q[j] );
*/
int diag(int *p,int n,int *q,int m)
{
  /*@
    loop invariant I: 0 <= i <= n ;
    loop invariant PI: 
      \forall integer i0; 0 <= i0 < i ==>
      \forall integer j0; 0 <= j0 < m ==>
        p[i0] < q[j0];
    loop assigns i;
  */
  for (int i = 0; i<n; i++)
    /*@
      loop invariant J: 0 <= j <= m ;
      loop invariant PJ:
        \forall integer j0; 0 <= j0 < j ==>
          p[i] < q[j0];
      loop assigns j;
    */
    for (int j = 0; j<m; j++)
      if (p[i] >= q[j]) return 0;
  return 1;
}
