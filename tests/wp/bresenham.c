/**************************************************************************/
/*                                                                        */
/* Proof of the Bresenham line drawing algorithm.                         */
/* (see examples/bresenham/ for the Why proof)                            */
/*                                                                        */
/* Jean-Christophe Filliâtre (LRI, Université Paris Sud)                  */
/* June 2008                                                              */
/*                                                                        */
/**************************************************************************/

int x2, y2;

/*@ axiomatic Abs {
  @   logic integer abs(integer x);
  @   axiom abs_def: 
  @     \forall integer x; 
  @       (x >= 0 && abs(x) == x) || (x <= 0 && abs(x) == -x);
  @ }
  @*/

/*@ predicate best(integer x2,integer y2,integer x, integer y) =
  @   \forall integer yp; abs(x2 * y - x * y2) <= abs (x2 * yp - x * y2)
  @ ; */

/*@ predicate Invariant(integer x2,integer y2,integer x, integer y, integer e) =
  @   e == 2 * (x + 1) * y2 - (2 * y + 1) * x2 &&
  @   2 * (y2 - x2) <= e <= 2 * y2
  @ ; */

/*@ lemma invariant_is_ok : 
  @  \forall integer x2,y2,x, y, e; Invariant(x2,y2,x,y,e) ==> best(x2,y2,x,y);
  @*/

//@ lemma z_ring_0 : \forall integer a, b, c; a * (b+c) == a*b + a*c;
//@ lemma z_ring_1 : \forall integer a, b, c; (b+c) * a == b*a + c*a;

//@requires 0 <= y2 <= x2;
void bresenham() {
  int x = 0;
  int y = 0;
  int e = 2 * y2 - x2;
  /*@ loop invariant INV: 0 <= x <= x2 + 1 && Invariant(x2,y2,x,y,e);
    @*/
  for (x = 0; x <= x2; x++) {
    // plot (x,y) at this point
    //@ assert BEST: best(x2,y2,x,y);
    if (e < 0)
      e += 2 * y2;
    else {
      y++;
      e += 2 * (y2 - x2);
    }
  }
}
