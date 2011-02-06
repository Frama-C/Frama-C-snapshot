/* run.config 
   DONTRUN: test under construction
*/

/*@ assigns \nothing;
    exits never: oracle_ok: \false ;
//    behavior found:
//      assumes \exists int k ; 0 <= k < 15 && t1[k] != t2[k] ; 
//      ensures oracle_ok: \result == 0 ;
//    behavior not_found:
//      assumes \forall int k ; 0 <= k < 15 ==> t1[k] == t2[k] ; 
//      ensures oracle_ok: \result == 1 ;
//   complete behaviors found, not_found ;
//   disjoint behaviors found, not_found ;
 */
int Type4Ex2 (int t1[], int t2[]) {
  int i ;

//  /*@ loop assigns i;
//    loop invariant I0: oracle_ok: 0 <= i <= 15 ; 
//    loop invariant not_yet_found: oracle_ok: \forall int k ; 0 <= k < i ==> t1[k] == t2[k] ; 
//   */
  for (i = 0 ; i < 15 ; i++)
    /*@ assigns \nothing;
     */
    if (t1[i] != t2[i])
      return 0 ;
  return 1 ;
}
