/*
  run.config_qualif
  OPT: -wp-prop="-qed_ko"
  OPT: -wp-steps 50 -wp-prop qed_ko
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

