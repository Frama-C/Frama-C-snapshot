/*run.config
 OPT: -wp-model Hoare
 OPT: -wp-model Typed+ref
*/

/*run.config_qualif
 OPT: -wp -wp-model Typed+ref -wp-par 1 -wp-prop qed_ok
*/

/* -------------------------------------------------------------------------- */
/* --- GOAL: Such as funvar is used for the two model, Store and Hoare    --- */
/* --- the oracles of this test have to been unchanged by any change in   --- */
/* --- funvar                                                             --- */
/* ---  TODO verify the semantics of startof: is it normal                --- */
/* ---  &G+i <> &(G[i]) ?                                                 --- */
/* -------------------------------------------------------------------------- */



int G [4];


/*@ ensures P_startof: G[0] == 0 ==> \result == &(G[0]) ;
 ensures P_addr: G[0] != 0 && G[1] == 0 ==> \result == &(G[1]) ;
*/
int * f (void)
{
  int i =0; 
  //@ loop assigns qed_ok: index:i ; 
  while (i < 4 && G[i] !=0) i++; 
  if (i>=4) return &(i) ; 
  else return &(G[i]); 
 
}


/*@ ensures P_startof_shift: G[0] == 0 ==> \result == &(G[0]) ;
 ensures P_addr_shift: G[0] != 0 && G[1] == 0 ==> \result == &(G[1]) ;
*/
int * f2 (void)
{
  int i =0; 
  //@ loop assigns qed_ok: index:i ; 
  while (((G+i) < (G+4)) && G[i] !=0) i++; 
  if (i>=4) return &(i) ; 
  else return &(G[i]); 
 
}



/*@ ensures P_addr_startof_shift: G[0] == 0 ==> \result == &(G[0]) ;
  ensures P_addr_addr_shift: G[0] != 0 && G[1] == 0 ==> \result == &(G[1]) ;
*/
int * g (void)
{
  int i =0; 
  //@ loop assigns qed_ok: i ; 
  while (((&G+i) < (&G+4)) && G[i] !=0) i++; 
  if (i>=4) return &(i) ; 
  else return &(G[i]); 
 
}
