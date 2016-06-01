/*@
  axiomatic Test {
  logic integer N ;
  predicate A ;
  predicate H(integer k) ;

  lemma GOAL:
    \let Hn = \forall integer i; 0 <= i < N ==> H(i) ;
    \let Dn = \exists integer i; 0 <= i < N && !H(i) ;
    (Hn ==> A) <==> (A || Dn) ;
 
  }
*/
