/* run.config_qualif
   OPT: -wp-prover native:coq
   OPT: -wp-prover native:alt-ergo -wp-steps 50 -wp-timeout 1
   OPT: -wp-prover alt-ergo -wp-steps 50 -wp-timeout 1
*/

//@ ensures KO: \result == 0.2 + x ;
float output(float x)
{
  return 0.2 + x ;
}
