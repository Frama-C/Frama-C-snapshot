/* run.config
   OPT: -wp-msg-key print-generated -wp-prover why3 -wp-gen
*/

/*@ requires \valid(t+(0..9));
    requires 0 <= x < 10;
    requires \forall integer n; 0 <= n < 10 ==> 0 <= t[n];
    ensures  \forall integer n; 0 <= n < 10 ==> 0 <= t[n];
  @*/
void f(int *t, int x){
  t[0] = t[x];
  t[1] = t[x];
  t[2] = t[x];
  t[3] = t[x];
  t[4] = t[x];
}
