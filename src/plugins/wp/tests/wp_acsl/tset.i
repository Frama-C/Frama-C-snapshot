/* run.config_qualif
   OPT: -wp -wp-prover alt-ergo,coq -wp-script tests/wp_acsl/tset.s
*/

/*@

  lemma UNION_EQ:
  \forall integer x,y ; 
  (\union(0,x) == \union(0,y)) <==> (x==y) ;

  lemma UNION_LIFT:
  \forall integer x,y ;
  \union(1,x) + \union(2,y) == \union(3,2+x,1+y,x+y);

  lemma UNION_RANGE:
  \forall integer k,n ; 0 <= k <= n ==>
  \union( (0..(k-1)) , k, ((k+1)..n) ) == (0..n) ;

  lemma UNION_DESCR:
  \forall integer n;
  { n-x | integer x; \subset(x,(0..n)) } == (0..n) ;

 */
