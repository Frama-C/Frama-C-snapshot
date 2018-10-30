/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="-qed_ko,-ko"
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="qed_ko,ko" -wp-steps 50
*/

/*@ ensures qed_ok: 0<=t<256 ;
  @ ensures qed_ko: 0<=t<128 ; */
void f(unsigned char t) {return;}
