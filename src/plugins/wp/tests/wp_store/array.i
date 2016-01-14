/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-proof alt-ergo -wp-par 1
*/

int G [4];

/*@ requires 0<=i<=3 && 0<=j<=3 ;
  @ ensures P_startof:    qed_ok: i<j  ==> \result == &(G[0]) ;
  @ ensures P_addr_shift: qed_ok: i>=j ==> \result == &(G[i]) ;
*/
int * g (int i,int j)
{
  if (i<j)
    return G ;
  else
    return G+i ;
}
 
