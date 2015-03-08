/*
  run.config_qualif
  OPT: -wp -wp-model Typed -wp-proof alt-ergo -wp-par 1 -wp-timeout 2 -wp-prop="-qed_ko"
  OPT: -wp -wp-model Typed -wp-proof alt-ergo -wp-par 1 -wp-timeout 2 -wp-prop qed_ko -wp-timeout 5
*/

/* -------------------------------------------------------------------------- */
/* --- GOAL: separation condition and interference with FunVar            --- */
/* -------------------------------------------------------------------------- */

/*@
   requires \valid(p); 
   requires \valid(q);
   requires 0<= *p < 200 && 0<= *q < 200;
   ensures qed_ok: P: \separated(p,q) ==> *p==\old(*p)+1;
   ensures qed_ok: Q: \separated(p,q) ==> *q==\old(*q)+1;
   ensures qed_ko: P_oracle_ko: *p==\old(*p)+1;
   ensures qed_ko: Q_oracle_ko: *q==\old(*q)+1;
 */
void f(int *p,int *q)
{
  *p+=1; *q+=1; 
}

