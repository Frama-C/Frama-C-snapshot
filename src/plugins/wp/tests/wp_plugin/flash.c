/* run.config
   OPT:
   OPT: -wp-driver tests/wp_plugin/flash.driver,tests/wp_plugin/flash-ergo.driver
   OPT: -wp-driver tests/wp_plugin/flash.driver -load-module tests/wp_plugin/flash.ml
*/

/* run.config_qualif
   OPT: -wp-timeout 1
   OPT: -wp-driver tests/wp_plugin/flash.driver,tests/wp_plugin/flash-ergo.driver
   OPT: -wp-driver tests/wp_plugin/flash.driver -load-module tests/wp_plugin/flash.ml
*/

/* -------------------------------------------------------------------------- */
/* --- Observation of Sequence of Reads and Writes                        --- */
/* -------------------------------------------------------------------------- */

//@ ghost int OBSERVER_time;

/*@
  axiomatic EVENT {
  type event = 
  | RdAt_int(int *)
  | WrAt_int(int *)
  ;
  logic \list<event> OBSERVER{L} reads OBSERVER_time;
  }
*/

/* -------------------------------------------------------------------------- */
/* --- Observation of memory access per pointer                           --- */
/* -------------------------------------------------------------------------- */

/*@
  axiomatic INDEX {
  type index;
  logic index INDEX_init ;
  }
*/

/* -------------------------------------------------------------------------- */
/* --- Instrumentation of read values via function RD                     --- */
/* -------------------------------------------------------------------------- */

//@ghost int RD_time ;

/*@ 
  axiomatic RD {
  logic index RD_current{L} reads RD_time;
  logic index RD_update( index idx , int *p ) reads \nothing;
  logic integer RD_access( index idx , int *p ) reads \nothing;
  logic int RD_value( int *p , integer k ) reads \nothing;
  }
*/

/*@
  ensures RD_current == RD_update(\old(RD_current),p);
  ensures RD_value( p , RD_access( \old(RD_current) , p ) ) == \result;
  ensures OBSERVER == ( \old(OBSERVER) ^ [| RdAt_int(p) |] );
  ensures OBSERVER_time == \old(OBSERVER_time)+1;
  assigns OBSERVER_time ;
  ensures RD_time == \old(RD_time)+1;
  assigns RD_time ;
 */
int RD(int *p);

/* -------------------------------------------------------------------------- */
/* --- Instrumentation of writen values via function WR                   --- */
/* -------------------------------------------------------------------------- */

//@ ghost int WR_time ;

/*@ 
  axiomatic WR {
  logic index WR_current{L} reads WR_time;
  logic index WR_update( index idx , int *p ) reads \nothing;
  logic integer WR_access( index idx , int *p ) reads \nothing;
  logic int WR_value( int *p , integer k ) reads \nothing;
  }
*/

/*@
  ensures WR_current == WR_update(\old(WR_current),p);
  ensures WR_value( p , WR_access( \old(WR_current) , p ) ) == v;
  ensures OBSERVER == ( \old(OBSERVER) ^ [| WrAt_int(p) |] );
  ensures OBSERVER_time == \old(OBSERVER_time)+1;
  assigns OBSERVER_time ;
  ensures WR_time == \old(WR_time)+1;
  assigns WR_time ;
 */
void WR(int *p,int v);

/* -------------------------------------------------------------------------- */
/* --- Function under Proof                                               --- */
/* -------------------------------------------------------------------------- */

int a;
int b;

/*@
  requires OBSERVER == [| |] ;
  requires RD_current == INDEX_init ;
  requires WR_current == INDEX_init ;
  ensures Events: OBSERVER == [| RdAt_int(&a), RdAt_int(&b) , WrAt_int(&b) , RdAt_int(&a) |] ;
  ensures A_reads:  RD_access( RD_current , &a ) == 2;
  ensures B_reads:  RD_access( RD_current , &b ) == 1;
  ensures B_writes: WR_access( WR_current , &b ) == 1;
  ensures ReadValues: \result == RD_value(&a,0) + RD_value(&a,1) + RD_value(&b,0) ;
  ensures WriteValues: WR_value(&b,0) == RD_value(&a,0) + RD_value(&b,0) ;
*/
int job(void)
{
  int s = 0;
  s += RD(&a);
  s += RD(&b);
  WR(&b,s);
  s += RD(&a);
  return s;
}
