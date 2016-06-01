/* run.config*
   STDOPT: #"-main locks0_good"
 */

/*@ ghost int ghost_loctable[100] ;*/

/*@ axiomatic Locked {
  @   predicate locked{L}(struct mutex *m);
  @      // reads m, ghost_loctable[..] ;
  @
  @   axiom locked_dummy_axiom_for_reads{L} :
  @      \forall struct mutex *m;
  @        locked(m) && ghost_loctable[0] == 0 ==>
  @           locked(m) && ghost_loctable[0] == 0 ;
  @ }
  @*/

/*@
  requires !(locked(m));
  ensures locked(m);
  assigns ghost_loctable[0..99];

 */
void acquire_lock(struct mutex *m);

/*@
  requires locked(m);
  ensures !(locked(m));
  assigns ghost_loctable[..];

 */
void release_lock(struct mutex *m);

/*@
  requires !(locked(m));
  assigns ghost_loctable[..];
  behavior success:
  ensures (\result != 0) ==> locked(m);

  behavior failure:
  ensures (\result == 0) ==> !(locked(m));

 */
int try_acquire_lock(struct mutex *m);

struct mutex *pmutex;

/*@ requires !(locked(pmutex)); */
void locks0_good(int flag)
{
    acquire_lock(pmutex);
    release_lock(pmutex);
}
