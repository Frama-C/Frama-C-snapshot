/* run.config_qualif
   OPT: -pp-annot -wp -wp-par 1 -wp-prop="-qed_ko"
   OPT: -pp-annot -wp -wp-par 1 -wp-prop qed_ko -wp-steps 50
*/

#define OK 33
#define KO 55

int k ;
int inp[5] ;
int out[5] ;

/*@ requires 0 <= k < 5 ;
  @ ensures \result == out[\old(k)] ; 
  @ ensures inp[\old(k)] == u ;
  @ ensures k == \old(k)+1 ;
  @ assigns k,inp[k] ;
  @ */
int g(int u);

/*@ requires k == 0 ;
  @ behavior FST_FAIL:
  @   assumes out[0] != OK ;
  @   ensures qed_ok: k == 1 ;
  @   ensures qed_ok: inp[0] == a ;
  @   ensures qed_ok: \result == KO ;
  @ behavior SND_FAIL:
  @   assumes out[0] == OK ;
  @   assumes out[1] != OK ;
  @   ensures qed_ok: k == 2 ;
  @   ensures qed_ok: inp[0] == a ;
  @   ensures qed_ok: inp[1] == b ;
  @   ensures qed_ok: \result == KO ;
  @ behavior SUCCESS:
  @   assumes out[0] == OK ;
  @   assumes out[1] == OK ;
  @   ensures qed_ok: k == 2 ;
  @   ensures qed_ok: inp[0] == a ;
  @   ensures qed_ok: inp[1] == b ;
  @   ensures qed_ok: \result == OK ;
  @ behavior ko:
  @   ensures qed_ko: inp[0] == b ;
  @ behavior ko_1:
  @   assumes out[0] == OK ;
  @   assumes out[1] == OK ;
  @   ensures qed_ko: k == 1 ;
  @   ensures qed_ko: inp[1] == a ;
  @   ensures qed_ko: \result == KO ;
  @   */
int f(int a,int b)
{
  int x ;
  int y ;
  x = g(a);
  if (x != OK) return KO ;
  y = g(b);
  if (y != OK) return KO ;
  return OK ;
}
