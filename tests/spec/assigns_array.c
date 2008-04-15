/*@ ghost int ghost_loctable[100] ;*/


// The specification below should be rejected
/*@
  requires \valid(ghost_loctable + m);
  requires !ghost_loctable[m];
  ensures ghost_loctable[m];
  assigns ghost_loctable;

 */
void acquire_lock(int m) { ghost_loctable[m]++; }

// The specification above should be accepted
/*@
  requires \valid(ghost_loctable + m);
  requires ghost_loctable[m]==1;
  ensures !ghost_loctable[m];
  assigns ghost_loctable[..];
 */
void release_lock(int m) { ghost_loctable[m]--; }
