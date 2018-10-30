/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-prop="-qed_ko"
   OPT: -wp -wp-par 1 -wp-prop qed_ko -wp-steps 50
*/

int f(int x) {
  int y=1;
  if (x) goto M;
 L:
  y=2;
 M: 
  //@ assert qed_ko: oracle_ko: \at(y,L) == 0 ;
  return y;
}


int g(int x) {
  int y=0;
  if (!x) goto M;
  y=1 ;
 L:
  y=2 ;
 M:
  if (x) {
    //@ assert qed_ok: ok: \at(y,L) == 1 ;
    return 1;
  }
  return 0;
}
