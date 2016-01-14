/*@
 axiomatic bits {
   logic integer c2fc4_lbtest (integer x, integer n) ;
   predicate c2fc4_btest (integer x, integer n) = c2fc4_lbtest (x, n) != 0;

   axiom btest_bnot: \forall integer x,n ;   n>0 ==> ((TRIGGER:c2fc4_btest(~x,n)) <==> !c2fc4_btest(x,n));
   axiom btest_bxor: \forall integer x,y,n ; n>0 ==> ((TRIGGER:c2fc4_btest(x^y,n)) <==> ((c2fc4_btest(x,n) ^^ c2fc4_btest(y,n))));
   axiom btest_bor:  \forall integer x,y,n ; n>0 ==> ((TRIGGER:c2fc4_btest(x|y,n)) <==> ((c2fc4_btest(x,n) || c2fc4_btest(y,n))));
   axiom btest_band: \forall integer x,y,n ; n>0 ==> ((TRIGGER:c2fc4_btest(x&y,n)) <==> ((c2fc4_btest(x,n) && c2fc4_btest(y,n))));
}


lemma bnot_sint8: \forall signed char x ;   (~x) == (signed char)(~x);
lemma bxor_sint8: \forall signed char x,y ; (x ^ y) == (signed char)(x ^ y);
lemma bor_sint8:  \forall signed char x,y ; (x | y) == (signed char)(x | y);
lemma band_sint8: \forall signed char x,y ; (x & y) == (signed char)(x & y);
lemma blsr_sint8: \forall signed char x,y ; y >= 0 ==> (x >> y) == (signed char)(x >> y);

lemma bor_uint8:  \forall unsigned char x,y ; (x | y) == (unsigned char)(x | y);
lemma band_uint8: \forall unsigned char x,y ; (x & y) == (unsigned char)(x & y);
lemma blsr_uint8: \forall unsigned char x,y ; y >= 0 ==> (x >> y) == (unsigned char)(x >> y);

*/
