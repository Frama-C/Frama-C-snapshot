/* run.config_qualif
   OPT: -wp -wp-prover coq -wp-script tests/wp_bts/bts_1174.s -wp-model +real
*/

/*@ requires -10. <= x && x <= 10.; */
void job(int a,double x)
{
  double y;
  if (a) {
    y = x * 2.;
    /*@ assert qed_ok: x >= 0. ==> y >= 0.; */ ; }
  return;
}
