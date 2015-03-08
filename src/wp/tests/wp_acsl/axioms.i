/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-par 1
*/

// Test for the instanciation of axioms with labels.
// The axiomatic A is equivalent (in spirit) to the definition of predicate Q.

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
  @ requires a<=b ;
  @ ensures P : todo: P(t,a,b) ;
  @ ensures Q : Q(t,a,b) ;
  @ assigns todo: t[a..b] ;
  @ */

void f(int *t , int a, int b)
{

  /*@ loop invariant Index: a<=i<=b+1 ;
    @ loop invariant Positive: \forall int k ; a<=k<i ==> t[k] > 0 ;
    @ loop assigns i,t[a..i-1] ;
    @ */
  for(int i=a; i<=b; i++) t[i] = 1 ;

}
