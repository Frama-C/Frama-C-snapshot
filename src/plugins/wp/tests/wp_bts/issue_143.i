/* run.config
   DONTRUN:
*/
/* run.config_qualif
   EXECNOW: chmod a-x ./tests/inexistant-prover
   OPT: -wp
   OPT: -wp -wp-prover "alt-ergo,native:coq" -wp-alt-ergo ./tests/inexistant-prover -wp-coqc ./tests/inexistant-prover
   OPT: -wp -wp-prover "alt-ergo" -wp-alt-ergo ./tests/inexistant-prover
   OPT: -wp -wp-prover "native:coq" -wp-coqc ./tests/inexistant-prover
*/
 
/*@
  axiomatic A {
  lemma ok_because_inconsistent: \forall integer x;  x > 0 ==> x < 0 ==> x == 0 ;
  }
*/

/*@
  axiomatic B {
  lemma ok_because_consistent: \forall integer x;  x > 0 ==> x*x > 0 ;
  }
*/
