/*@ ghost int ghost_loctable[100] ;*/


// The specification below should be rejected
/*@
  requires \valid(ghost_loctable + m);
  requires !ghost_loctable[m];
  ensures ghost_loctable[m];
  assigns ghost_loctable;

 */
void acquire_lock(int m) { /*@ ghost ghost_loctable[m]++; */ }

// The specification above should be accepted
/*@
  requires \valid(ghost_loctable + m);
  requires ghost_loctable[m]==1;
  ensures !ghost_loctable[m];
  assigns ghost_loctable[..];
 */
void release_lock(int m) { /*@ ghost ghost_loctable[m]--; */ }



int Tab[10];
/*@ requires n < 10 ;
    behavior foo:
       assumes reset;
       assigns Tab[0..n-1];
    behavior bar: 
       assumes !reset;
       assigns \nothing;
*/
int h(int reset, int n) {
  int i, r = 0 ;
  /*@
    for foo:
      loop assigns Tab[0..i];
    for bar:
      loop assigns \nothing;
  */
  for (i = 0 ; i < n ; i++) {
    r += Tab[i] ;
    if (reset)
      Tab[i] = 0 ;
   }
  return r ;
}
    

