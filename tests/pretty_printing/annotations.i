/*@ axiomatic A {
    predicate P(integer x) reads \nothing ;
  }
*/

//@ ghost int global_decl ;
//@ ghost int global_def = 42 ;

/*@
  requires P(x) && x > 0 ;
  ensures P(x) ;
*/
void function_no_ghost(int x) {
  int y = 0;

  /*@ loop invariant 0 <= y <= x ;
      loop assigns y ;
      loop variant x - y ; */
  while (y < x) {
    /*@ assert y < x ; */
    y++;
    /*@ assert y <= x ; */
  }

  //@ assert y == x ;

  /*@ requires y == x ;
      assigns y ;
      ensures y != x ;
  */ {
    y -- ;
    y *= 2 ;
  }

  //@ requires y >= 0 ;
  y /= y ;
}

/*@
  requires P(x) && x > 0 ;
  ensures P(x) ;
*/
void function_with_ghost(int x) {
  //@ ghost int y = 0;

  /*@ ghost
    /@ loop invariant 0 <= y <= x ;
       loop assigns y ;
       loop variant x - y ; @/
    while (y < x) {
      /@ assert y < x ; @/
      y++;
      /@ assert y <= x ; @/
    }
  */

  //@ assert y == x ;

  /*@ ghost
    /@ requires y == x ;
       assigns y ;
       ensures y != x ;
    @/ {
      y -- ;
      y *= 2 ;
    }
  */

  /*@ ghost
    //@ requires y >= 0 ;
    y /= y ;
  */
}

/*@ ghost
  /@
    requires P(x) && x > 0 ;
    ensures P(x) ;
  @/
  void ghost_function(int x) {
    int y = 0;

    /@ loop invariant 0 <= y <= x ;
       loop assigns y ;
       loop variant x - y ; @/
    while (y < x) {
      /@ assert y < x ; @/
      y++;
      /@ assert y <= x ; @/
    }
    
    /@ assert y == x ; @/

    /@ requires y == x ;
       assigns y ;
       ensures y != x ;
    @/ {
      y -- ;
      y *= 2 ;
    }

    //@ requires y >= 0 ;
    y /= y ;
}
*/

/*@ ghost
  void function_declaration(int variable) ;
*/

void reference_function(void){
  //@ ghost function_declaration(42) ;
}
