/* run.config_qualif
   OPT: -wp-prover coq
   OPT: -wp-prover alt-ergo -wp-timeout 1
   OPT: -wp-prover why3:alt-ergo -wp-timeout 1
*/

//@ ensures KO: \result == 0.2 + x ;
float output(float x)
{
  return 0.2 + x ;
}
