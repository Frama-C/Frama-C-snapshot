/* run.config
   COMMENT: spec with floats and reals
*/

/*@ requires \valid(Mtmax_in);
  @ requires \valid(Mwmax);
  @ requires \valid(Mtmax_out);

  @ behavior OverEstimate_Motoring:
  @ assumes \true;
  @ ensures *Mtmax_out != *Mtmax_in + (5 - (((5 / 80) * *Mwmax) * 0.4));
  @*/
void foo(float* Mtmax_in, float* Mwmax, float* Mtmax_out) {
  /* Real semantics and floating-point semantics give different results because:
     1) Mtmax_out is 5.975 is in R
     2) The float closest to 5.975 is 5.97499999999999964472863211995 */
  *Mtmax_out = *Mtmax_in + (5 - (((5 / 80) * *Mwmax) * 0.4));
}

/*@ requires \valid(Mtmin_in);
  @ requires \valid(Mwmin);
  @ requires \valid(Mtmin_out);
  @
  @ behavior UnderEstimate_Motoring:
  @ assumes \true;
  @ ensures *Mtmin_out == *Mtmin_in < 0.85 * *Mwmin ? *Mtmin_in : 0.85 * *Mwmin;
  @*/
void bar(float* Mtmin_in, float* Mwmin, float* Mtmin_out) {
  *Mtmin_out = 0.85 * *Mwmin;
}

int main(void) {
  float f = 1.0, g = 1.0, h;
  foo(&f, &g, &h);
  bar(&f, &g, &h);
  return 0;
}
