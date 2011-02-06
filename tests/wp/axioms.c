/* run.config 
   OPT: -wp -wp-assigns effect -wp-axioms -wp-print
*/
/* run.config_why
   OPT: -wp -wp-assigns effect -wp-axioms -wp-timeout 5 -wp-proof alt-ergo -wp-par 1

*/

// Test for the instanciation of axioms with labels.
// The axiomatic A is equivalent (in spirit) to the definition of predicate Q.
// Both should be provable with alt-ergo immediately, under the hypothesis of the loop-assign.
// This last property can be discharged in Coq with the provided proof.


/*@ axiomatic A {
  @ predicate P{L}(int *t,int a,int b) reads *t ;
  @ axiom D{L}:
  @   \forall int * t ; \forall int a,b ;
  @   (\forall int k ; a<=k<=b ==> \valid(t+k) ==> t[k] > 0) ==> P(t,a,b) ;
  @ }
  @ */

/*@ predicate Q(int *t,int a,int b) = 
  @   \forall int k ; a<=k<=b ==> \valid(t+k) ==> t[k] > 0 ;
  @ */


/*@ requires \valid(t+(a..b)) ;
  @ ensures P : P(t,a,b) ;
  @ ensures Q : Q(t,a,b) ;
  @ assigns t[a..b] ;
  @ */

void f(int *t , int a, int b)
{

  /*@ loop invariant Index: a<=i<=b+1 ;
    @ loop invariant Positive: \forall int k ; a<=k<i ==> t[k] > 0 ;
    @ loop assigns i,t[a..i-1] ;
    @ */
  for(int i=a; i<=b; i++) t[i] = 1 ;

}

/* -------------------------------------------------------------------------- */
/* --- Coq Proof for loop assigns                                         --- */
/* -------------------------------------------------------------------------- */

/*
Proof.
intros. unfold ze1. apply inc_union_left.
rewrite right_empty. unfold ze0. unfold ze. 
repeat ( try rewrite union_assoc ; try rewrite right_empty ; try rewrite left_empty ).
apply inc_union_union.
   apply inc_same.
   apply inc_union_left. unfold address_zone. unfold address_range.
   repeat (try rewrite addr_base ; try repeat rewrite addr_offset ; repeat rewrite addr_sizeof ).
   rewrite inc_range_range. intros. split. auto. split. omega. unfold i. omega.
   unfold address_zone. unfold address_range.
   repeat (try rewrite addr_base ; try repeat rewrite addr_offset ; try rewrite addr_sizeof).
   rewrite inc_range_range. intros. split.
   unfold address_shift. rewrite addr_base. auto. split. unfold address_shift.
   rewrite addr_offset. omega. unfold address_shift. rewrite addr_offset. unfold i. omega.
   rewrite right_empty. apply inc_union_right. left. apply inc_same.
Save.
*/

