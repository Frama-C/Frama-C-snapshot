/* run.config_qualif
   OPT: -wp -wp-prop A -then -wp-prop B
*/

/* -------------------------------------------------------------------------- */
/* - Testing that definition of P is correctly generated for both WP rounds - */
/* -------------------------------------------------------------------------- */

//@ predicate P(integer x) = 0 < x ;

int x,a,b ;

/*@ requires P(x) ;
  @ ensures A: P(a) ;
  @ ensures B: P(b) ;
*/
void f(void) {
  a = x+1 ;
  b = x+2 ;
}
