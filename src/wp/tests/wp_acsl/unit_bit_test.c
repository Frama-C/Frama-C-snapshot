/* run.config
   STDOPT: +"-pp-annot"
*/

/* run.config_qualif
   STDOPT: +"-pp-annot"
*/

#define ST(a) (((a)&2)!=0)

/*@ 
  ensures ok: ST(\result) <==> ST(a) || ST(b) ; 
  ensures ko: ST(\result) <==> ST(a) && ST(b) ; 
*/
char sum( char a , char b )
{ return a|b; }

#define BIT_TEST(x,n) (((x)&(1<<(n)))!=0)
/*@ ensures bit_zero: 
      BIT_TEST(\result,0) == BIT_TEST(x,31);
  @ ensures other_bits: 
      \forall int k ; 0 <= k && k < 31
      ==> ( BIT_TEST(\result,1+k) <==>  BIT_TEST(x,k));
 */
unsigned rotate_left (unsigned x) {
  return (x << 1) | (x >> 31);
}
