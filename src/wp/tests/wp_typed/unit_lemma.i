/* run.config_qualif
   OPT:
*/

/*@ axiomatic D1 {
    predicate P1(integer x) ;
    predicate P2(integer x) ;
    predicate P3(integer x) ;
    predicate P4(integer x) ;
    axiom P12: \forall integer x ; P1(x) ==> P2(x) ;
  }
*/

/*@ axiomatic Poluted {
    predicate H(integer x) ;
    lemma Foo: 
      H(0) ==>
      (\forall integer x ; x<0 ==> H(x)) ==>
      (\forall integer x ; x>0 ==> H(x)) ==>
      (\forall integer x ; H(x)) ;
    }
*/

/*@ lemma P23_KO: \forall integer x ; P2(x) ==> P3(x) ; */

/*@ axiomatic G1 {
    lemma P13: \forall integer x ; P1(x) ==> P3(x) ;
    axiom P34: \forall integer x ; P3(x) ==> P4(x) ;
    lemma P14: \forall integer x ; P1(x) ==> P4(x) ;
    }
*/

/*@ predicate P5(integer x) = P1(x) && P2(x) ; */

/*@ lemma P52: \forall integer x ; P5(x) ==> P2(x) ; */

/*@ axiomatic G2 {
    lemma P54: \forall integer x ; P5(x) ==> P4(x) ;
    }
*/
