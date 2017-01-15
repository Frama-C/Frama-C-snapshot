/* run.config
   OPT: -ulevel=1 -wp -wp-prop=@ensures -wp-prover script -session tests/wp_plugin/unroll
*/

/* run.config_qualif
   DONTRUN:
*/

enum {Max = 16};

/*@ predicate zeroed (unsigned *p, integer a, integer b) =
  @   \forall integer k; a <= k <= b ==> p[k]==0 ;
*/
/*@ requires access: \valid(&t[0 .. (Max -1)]);
  @ assigns t[0 .. (Max -1)];
  @ ensures zero: zeroed(t,0,Max-1);
 */
void unrolled_loop(unsigned *t){
  //@ loop pragma UNROLL "completely", Max+1;
  for (unsigned i=0; i<Max; i++) t[i] = 0;
}
