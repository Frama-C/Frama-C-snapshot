/* run.config
   OPT:-wp-prover=why3 -wp-gen -wp-msg-key print-generated
*/
/* run.config_qualif
   DONTRUN:
*/

/*@ predicate P(integer i) = i == 42 ; */
/*@ predicate RP(integer i) = (i <= 0) || ( P(i) && RP(i-1) ) ; */

/*@ logic integer F(integer i) = i * 2 ; */
/*@ logic integer RF(integer i) = (i <= 0) ? 0 : F(i) + RF(i-1) ; */

/*@ lemma foo: \forall integer i ; i > 0 ==> RP(RF(i)) ; */