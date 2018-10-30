/* run.config
   OPT: -wp -wp-prover none
*/

/* run.config_qualif
   OPT: -load-module tests/wp_tip/TacNOP.ml -wp -wp-par 1 -wp-prover script -session tests/wp_tip/tac_split_quantifiers
*/


/*@ axiomatic A {
   logic integer A reads \nothing ;
   predicate P1(integer a1) reads \nothing ;
   predicate Q1(integer a1) reads \nothing ;
   predicate R1(integer a1) reads \nothing ;
   predicate P2(integer a1, integer a2) reads \nothing ;
   predicate Q2(integer a1, integer a2) reads \nothing ;
   predicate R2(integer a1, integer a2) reads \nothing ;
   } */

/*@ ensures Goal_Exist_Or: 
       (\exists integer a, b, c ;
        P1(a) || P2(b, c) || Q1(1) || \exists integer d ; Q2(a,d)) ;

  @ ensures Goal_Exist_And:
       (\exists integer a, b, c ;
        P1(a) && P2(b, c) && Q1(c) && \exists integer d ; Q2(a,d)) ;

  @ ensures Goal_Exist_And_bis:
       (\exists integer a, b, c, d ;
        P2(a, b) && R2(b, c) && Q1(1) && \exists integer e ; Q2(d,e)) ;

  @ ensures Hyp_Forall_And: 
       (\forall integer a, b, c ;
        P1(a) && P2(b, c) && Q1(1) && \forall integer d ; Q2(a,d)
       ) ==>
       Q1(A);

 @ ensures Hyp_Forall_Or_bis:
       (\forall integer a, b, c, d ;
        P2(a, b) || R2(b, c) || Q1(1) || \forall integer e ; Q2(d,e)
       ) ==>
       Q1(A);
*/
void split(void) { ; }
