/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-prop qed_ok
*/

/*@ requires \valid(t + (a..b));
  @ requires a <= b ;
  @ ensures qed_ok: \forall int i ; a <= i <= b ==> t[i] == e ;
  @ assigns qed_ok: t[a..b] ;
  @*/
void init( int * t , int a , int b , int e )
{
  /*@ loop invariant qed_ok: a <= i <= b+1 ;
    @ loop invariant qed_ok: \forall int j ; a <= j < i ==> t[j] == e ;
    @ loop assigns qed_ok: i,t[a..i-1] ; */
  for ( int i = a ; i <= b ; i ++ ) 
    t[i] = e ;
}
