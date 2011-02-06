/* run.config
   OPT: -wp -wp-model Hoare -wp-print
 */

// ORACLES FAUX pour les preuves des preconditions de main.
// On attend le but Start => A => U
// On obtient les buts Start, A et U

//@ predicate P(integer x) ;

int x = 0;

/*@ requires Start: P(x) ; 
  @ ensures Final: P(x+1) ;
  @ behavior useless:
  @   assumes  A: P(x+2) ;
  @   requires U: P(x+3) ;
  @   ensures  B: P(x+4) ;
  @ */
int main(void) {

}
