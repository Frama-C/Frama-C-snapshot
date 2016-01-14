/* run.config
   OPT:
*/
/* run.config_qualif
   OPT: -wp-prop="-ko"
   OPT: -wp-prop="ko"
*/



#include "unit_bitwise.h"
//===============================================
//-- int
//===============================================

/*@ ensures ok: \result == (a & b); 
*/
int band_int(int a,int b) { 
  //@ assert ok: (a & 172) <= 281 ;
  //@ assert ko: (a & 281) <= 172 ;
  return a & b ; 
}

/*@ ensures ok: \result == (a | b); 
 */
int bor_int(int a,int b) { return a | b ; }

/*@ ensures ok: \result == (a ^ b); 
 */
int bxor_int(int a,int b) { return a ^ b ; }

/*@ ensures ok: \result == (~a) ;
 */
int bnot_int(int a) { return ~a ; }

/*@ ensures ok: \result == (int) (a << n) ;
 */
int lshift_int(int a,int n) { return a << n ; }

/*@ ensures ok: \result == (a >> n) ;
 */
int rshift_int(int a,int n) { return a >> n ; }

//===============================================
//-- unsigned int 
//===============================================
typedef unsigned int uint;

void band1_uint(uint a) {
   uint b = a & 1;
   //@ assert ok: b == 0 || b == 1;
   //@ assert ok: 0 <= b <= 1;
}

/*@ ensures ok: \result == (a & b); 
*/
uint band_uint(uint a,uint b) { return a & b ; }

/*@ ensures ok: \result == (a | b); 
 */
uint bor_uint(uint a,uint b) { return a | b ; }

/*@ ensures ok: \result == (uint)(a ^ b); 
    ensures ko: \result == (a ^ b); 
 */
uint bxor_uint(uint a,uint b) { return a ^ b ; }

/*@ ensures ok: \result == (uint)(~a) ;
    ensures ko: \result == (~a) ;
 */
uint bnot_uint(uint a) { return ~a ; }

/*@ ensures ok: \result == (uint) (a << n) ;
 */
uint lshift_uint(uint a,uint n) { return a << n ; }

/*@ ensures ok: \result == (a >> n) ;
 */
uint rshift_uint(uint a,uint n) { return a >> n ; }

//===============================================
//-- char
//===============================================

/*@ ensures ok: \result == (a & b); 
*/
char band_char(char a,char b) { return a & b ; }

/*@ ensures ok: \result == (a | b); 
 */
char bor_char(char a,char b) { return a | b ; }

/*@ ensures ok: \result == (a ^ b); 
 */
char bxor_char(char a,char b) { return a ^ b ; }

/*@ ensures ok: \result == (~a) ;
 */
char bnot_char(char a) { return ~a ; }

/*@ ensures ok: \result == (char) (a << n) ;
 */
char lshift_char(char a,char n) { return a << n ; }

/*@ ensures ok: \result == (char) (a >> n) ;
 */
char rshift_char(char a,char n) { return a >> n ; }

//===============================================
//-- unsigned char
//===============================================
typedef unsigned char uchar;

void band1_uchar(uchar a) {
   uchar b = a & 1;
   //@ assert ok: b == 0 || b == 1;
   //@ assert ok: 0 <= b <= 1;
}

/*@ ensures ok: \result == (a & b); 
*/
uchar band_uchar(uchar a,uchar b) { return a & b ; }

/*@ ensures ok: \result == (a | b); 
 */
uchar bor_uchar(uchar a,uchar b) { return a | b ; }

/*@ ensures ok: \result == (uchar)(a ^ b); 
    ensures ko: \result == (a ^ b); 
 */
uchar bxor_uchar(uchar a,uchar b) { return a ^ b ; }

/*@ ensures ok: \result == (uchar)(~a) ;
    ensures ko: \result == (~a) ;
 */
uchar bnot_uchar(uchar a) { return ~a ; }

/*@ ensures ok: \result == (uchar) (a << n) ;
 */
uchar lshift_uchar(uchar a,uchar n) { return a << n ; }

/*@ ensures ok: \result == (a >> n) ;
 */
uchar rshift_uchar(uchar a,uchar n) { return a >> n ; }

//===============================================
//-- Logic
//===============================================
/*@ lemma land_assoc: \forall integer a,b,c; (a & (b & c)) == ((a & b) & c) ;
  @ lemma land_com: \forall integer a,b; (a & b) == (b & a) ;
  @ lemma land_abs: \forall integer a; (a & 0) == 0 ;
  @ lemma land_stb: \forall integer a; (a & a) == a ;
  @ lemma lor_assoc: \forall integer a,b,c; (a | (b | c)) == ((a | b) | c) ;
  @ lemma lor_com: \forall integer a,b; (a | b) == (b | a) ;
  @ lemma lor_neu: \forall integer a; (a | 0) == a ;
  @ lemma lor_stb: \forall integer a; (a | a) == a ;
  @ lemma lxor_assoc: \forall integer a,b,c; (a ^ (b ^ c)) == ((a ^ b) ^ c) ;
  @ lemma lxor_com: \forall integer a,b; (a ^ b) == (b ^ a) ;
 */
//===============================================

void band1_ushort(unsigned short a) {
   unsigned short b = a & 1;
   //@ assert ok: b == 0 || b == 1;
   //@ assert ok: 0 <= b <= 1;
}
void band1_ulong(unsigned long a) {
   unsigned long b = a & 1;
   //@ assert ok: b == 0 || b == 1;
   //@ assert ok: 0 <= b <= 1;
}

/*@ ensures ok: \result == c ;
 */
uchar cast(uchar c,uint i,long long j) { 
  //@ assert ok: ((uint)((int) i)) == i ;
  //@ assert ok: ((int)((uint) i)) == (int)i ;
  //@ assert ok: ((uchar)((int) i)) == (uchar)i ;

  //@ assert ok: ((int)((uchar) c)) == (uchar)c ;
  //@ assert ok: ((int)((char) c))  == (char)c ;
  //@ assert ok: ((uint)((uchar) c)) == (uchar)c ;

  //@ assert ok: ((uint)(j & 3)) == (j & 3) ;

  //@ assert ko: ((uint)((char) c)) == (char)c ;

  return (uint) c ; 
}

