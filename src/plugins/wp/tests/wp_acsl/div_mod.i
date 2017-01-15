/* run.config
   OPT: -wp-gen -wp-print
*/
/* run.config_qualif
   OPT: -wp-prop="-ko"
   OPT: -wp-prover why3:alt-ergo -wp-prop="-ko"
   OPT: -wp-prover "alt-ergo" -wp-prop="ko" -wp-timeout 5
*/

//@ axiomatic Eq { predicate Peq(integer x,integer y) = x == y ; } 

/*@
  @ ensures  d0: div_pos_pos:      Peq(   5  /   3 ,  1 );
  @ ensures  d1: div_neg_pos:      Peq( (-5) /   3 , -1 );
  @ ensures  d2: div_pos_neg:      Peq(   5  / (-3), -1 );
  @ ensures  d3: div_neg_neg:      Peq( (-5) / (-3),  1 );
  @ ensures  d4: div_x_1:          Peq(   x  /   1,   x );
  @ ensures  d5: div_x_minus1:     Peq(   x  / (-1), -x );
  @ ensures  d6: div_0_x: x!=0 ==> Peq(   0  /   x,   0 );
  @ ensures  d7: div_0_x: ko:      Peq(   0  /   x,   0 );

  @ ensures  sd0: div_pos_pos: x>=0 && y>0 ==> (x / y) >= 0;
  @ ensures  sd1: div_neg_pos: x<=0 && y>0 ==> (x / y) <= 0;
  @ ensures  sd2: div_pos_neg: x>=0 && y<0 ==> (x / y) <= 0;
  @ ensures  sd3: div_neg_neg: x<=0 && y<0 ==> (x / y) >= 0;

  @ ensures  m0: mod_pos_pos:      Peq(   5  %   3 ,  2 );
  @ ensures  m1: mod_neg_pos:      Peq( (-5) %   3 , -2 );
  @ ensures  m2: mod_pos_neg:      Peq(   5  % (-3),  2 );
  @ ensures  m3: mod_neg_neg:      Peq( (-5) % (-3), -2 );
  @ ensures  m4: mod_x_1:          Peq(   x  %   1,   0 );
  @ ensures  m5: mod_x_minus1:     Peq(   x  % (-1),  0 );
  @ ensures  m6: mod_0_x: x!=0 ==> Peq(   0  %   x,   0 );
  @ ensures  m7: mod_0_x: ko:      Peq(   0  %   x,   0 );

  @ ensures  sm0: mod_pos_pos: x>=0 && y>0 ==> (x % y) >= 0;
  @ ensures  sm1: mod_neg_pos: x<=0 && y>0 ==> (x % y) <= 0;
  @ ensures  sm2: mod_pos_neg: x>=0 && y<0 ==> (x % y) >= 0;
  @ ensures  sm3: mod_neg_neg: x<=0 && y<0 ==> (x % y) <= 0;

  @*/
void f(int x, int y) { ; }
