/* run.config_qualif
  OPT: -wp-prop="-qed_ko"
  OPT: -wp-prop qed_ko -wp-steps 50
*/

/*@ ensures qed_ko: KO:\result < 0;
  @ ensures qed_ok: nat:\result >= 0;
  @*/
unsigned int cast_sgn_usgn (void) {
  int x = -1;
  return x;
}


unsigned char uchar_range (unsigned char i) {
  //@ assert qed_ok: A1:i >= 0;
  //@ assert qed_ok: A2:i <= 255;
  return i;
}

//@ lemma ucL1: qed_ok: (unsigned char) 255 == 255 ;
//@ lemma ucL2: qed_ok: (unsigned char) 256 == 0 ;
//@ lemma ucL3: qed_ok: (unsigned char)  -1 == 255 ;
//@ lemma ucL4: qed_ok: (unsigned char) 1023 == (unsigned char) 255 ;

//@ lemma ucN1: qed_ok: (unsigned char) 256 == 256 ==> \false;
//@ lemma ucN2: qed_ok: (unsigned char) 256 == -1  ==> \false ;

//@ lemma scL1: qed_ok: (signed char) 127 == 127 ;
//@ lemma scN1: qed_ok: (signed char) 255 == 255 ==> \false;
//@ lemma scN2: qed_ok: (signed char) 256 == 255 ==> \false ;

//@ lemma L01: lnot: qed_ok: ~(-1) == 0 ;

//@ lemma L10: land: neutral:   qed_ok: (55 & -1) == 55 ;
//@ lemma L11: land: absorbant: qed_ok: (55 & 0)  == 0 ;
//@ lemma L12: land:            qed_ok: (3  & 2)  == 2;
//@ lemma L13: land:            qed_ok: (55 & 7 & 1)   == 1;

//@ lemma L20: lor:  neutral:   qed_ok: (55 | 0)  == 55 ;
//@ lemma L21: lor:  absorbant: qed_ok: (55 | -1) == (-1) ;

//@ lemma L30: lxor_neutral: qed_ok: (55 ^ 0) == 55 ;

//@ axiomatic undefined_x { logic integer x ; }

//@ lemma L14: land: absorbant: qed_ok: (1 & x & 2)  == 0 ;
//@ lemma L15: land:            qed_ok: (3 & x & 2)  == (2 & x) ;
//@ lemma L16: land:            qed_ok: (3 & x & -2) == (2 & x) ;


/*@ lemma ASSOC: land: qed_ok:
  \forall integer x,y,z,t; x == (y & z) ==> (x & t) == (y & t & z) ;
*/
