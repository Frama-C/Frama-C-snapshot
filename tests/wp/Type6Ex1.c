/* run.config 
   DONTRUN: test under construction
*/

/*@ axiomatic DISTANCE_INT {
  @   logic int Distance_int (int x, int y) ;
  @   axiom r:
        \forall int x ;
          Distance_int (x,x) == 0 ;
  @   axiom s:
        \forall int x, y ;
          Distance_int (x,y) + Distance_int (y,x) == 0 ;
  @ }
  @*/

/*@ assigns \nothing ;
    ensures result: \result == Distance_int (val1, val2) ;
    exits   never: \false ;
  @*/
int distance_int (int val1, int val2) ;

/*@ assigns \nothing ;
    exits never: oracle_ok: \false ;
    
  @ behavior not_found:
      assumes \forall int k ; 0 <= k < 10 ==> 0 == Distance_int (tab[k], tabref[k]) ;
      ensures oracle_ok: \result == 1 ;

  @ behavior found:
      assumes \exists int k ; 0 <= k < 10 && 0 != Distance_int (tab[k], tabref[k]) ;
      ensures oracle_ok: \result == 0 ;

  @ complete behaviors not_found, found ;
  @ disjoint behaviors not_found, found ;
  @*/
char Type6Ex1(int tab [10], int tabref[10]) {
  int i = 0 ;
  char control_ok = 1 ;    

  /*@ loop assigns i, control_ok ;
      loop invariant I0: oracle_ok: 0 <= i <= 10 ;
      loop invariant C0: oracle_ok: 0 == control_ok || control_ok == 1 ;
      loop invariant I0_C0: oracle_ok: 0 == control_ok ==> i < 10 ;
      loop invariant not_yet_found: oracle_ok: 
        control_ok==1 ==> \forall int k ; 0 <= k < i ==> 0 == Distance_int (tab[k], tabref[k]) ;
      loop invariant not_found_before: oracle_ok: 
        control_ok==0 ==> 0 != Distance_int (tab[i], tabref[i]) ;
    @  loop variant decr: oracle_ok: (control_ok==1) ? (10 - i) : 0;
    @*/
   while (i < 10 && control_ok)
    if (0 == distance_int (tab[i], tabref[i]))
      i++ ;
    else
      control_ok = 0 ;
  return control_ok ;
}

