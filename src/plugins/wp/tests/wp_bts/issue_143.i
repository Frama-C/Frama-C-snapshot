/* run.config
   DONTRUN:
*/
/* run.config_qualif
   EXECNOW: chmod a-x ./tests/inexistant-prover
   OPT: -wp
   OPT: -wp -wp-prover "alt-ergo,coq,why3:alt-ergo" -wp-alt-ergo ./tests/inexistant-prover -wp-coqc ./tests/inexistant-prover -wp-why3 ./tests/inexistant-prover 
   OPT: -wp -wp-prover "alt-ergo" -wp-alt-ergo ./tests/inexistant-prover
   OPT: -wp -wp-prover "coq" -wp-coqc ./tests/inexistant-prover
*/
 
/*@
  axiomatic With_inconsistent_antecedent {
  lemma with_inconsistent_antecedent: ok: \forall integer x;  2*x > 0 ==> x < 0 ==> x==0 ;
  }
*/

/*@
  axiomatic With_consistent_antecedent {
  lemma with_consistent_antecedent: ok: \forall integer x;  x > 0 ==> x*x>0 ;
  }
*/
