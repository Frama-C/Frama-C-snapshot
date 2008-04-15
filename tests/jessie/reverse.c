

/* in-place list reversal */

#include "list.h"

/*@ requires null_term_list(p0);
  @ ensures \forall list l0; 
  @   \old(list_contents(p0, l0)) ==> list_contents(\result, rev(l0));
  @*/
linked_list list_reverse(linked_list p0) {
  linked_list r = p0;
  linked_list p = (linked_list)NULL;
  /*@ loop invariant 
    @   \exists list lp; \exists list lr;
    @      list_contents(p, lp) && 
    @      list_contents(r, lr) && 
    @      disjoint(lp, lr) &&
    @      \forall list l; 
    @        \at(list_contents(p0, l), Pre) ==> app(rev(lr), lp) == rev(l);
    @ // loop variant length(r) for length_order 
    @*/
  while (r != (linked_list)NULL) {
    linked_list q = r;
    r = r->tl;
    q->tl = p;
    p = q;
  }
  return p;
}


/* 
Local Variables:
compile-command: "LC_ALL=C make reverse"
End:
*/
