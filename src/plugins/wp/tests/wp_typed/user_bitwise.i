/* run.config
   OPT: -wp-model +ref -wp-prop="-ko"
   OPT: -wp-model +ref -wp-prop="ko"
*/
/* run.config_qualif
   OPT: -wp-model +ref -wp-prop="-ko"
   OPT: -wp-model +ref -wp-prop="ko"
 */

/*---------------------------------------------------*/
/*@ ensures b0: 
      (\result&(1<<0))!=0 <==> (x&(1<<31))!=0;
  @ ensures bk: 
      \forall int k ; 0 <= k && k < 31
      ==> ( (\result&(1<<(1+k)))!=0 <==> (x&(1<<k))!=0 );
 */
unsigned rl1 (unsigned x) {
  return (x << 1) | (x >> 31);
}

/*@ ensures b0: 
      (\result&(1<<31))!=0 <==> (x&(1<<0))!=0;
  @ ensures bk: 
      \forall int k ; 0 <= k && k < 31
      ==> ( (\result&(1<<k))!=0 <==> (x&(1<<(1+k)))!=0 );
 */
unsigned rr1 (unsigned x) {
  return (x >> 1) | (x << 31);
}

/*---------------------------------------------------*/
/*@ requires r: 0 < n < 32 ;
  @ ensures b1: 
      \forall int k ; 0 <= k && k < n
      ==> ( (\result&(1<<k)) <==> (x&(1<<(32-n+k))) );
  @ ensures b2: 
      \forall int k ; 0 <= k && k < 32-n
      ==> ( (x&(1<<k)) <==> (\result&(1<<(k+n))) );
 */
unsigned rln32 (unsigned x, int n) {
  return (x << n) | (x >> (32 - n));
}

/*@ requires r: 0 < n < 32 ;
  @ ensures b1: 
      \forall int k ; 0 <= k && k < n
      ==> ( (\result&(1<<(32-n+k))) <==> (x&(1<<k)) );
  @ ensures b2: 
      \forall int k ; 0 <= k && k < 32-n
      ==> ( (\result&(1<<k)) <==> (x&(1<<(n+k))) );
 */
unsigned rrn32 (unsigned x, int n) {
  return (x << (32 - n)) | (x >> n);
}

/*---------------------------------------------------*/
/*@ requires r: 0 < n < 64 ;
  @ ensures b1: 
      \forall int k ; 0 <= k && k < n
      ==> ( (\result&(1<<k)) <==> (x&(1<<(64-n+k))) );
  @ ensures b2: 
      \forall int k ; 0 <= k && k < 64-n
      ==> ( (x&(1<<k)) <==> (\result&(1<<(k+n))) );
 */
unsigned long long rln64 (unsigned long long x, int n) {
  return (x << n) | (x >> (64 - n));
}

/*@ requires r: 0 < n < 64 ;
  @ ensures b1: 
      \forall int k ; 0 <= k && k < n
      ==> ( (\result&(1<<(64-n+k))) <==> (x&(1<<k)) );
  @ ensures b2: 
      \forall int k ; 0 <= k && k < 64-n
      ==> ( (\result&(1<<k)) <==> (x&(1<<(n+k))) );
 */
unsigned long long rrn64 (unsigned long long x, int n) {
  return (x << (64 - n)) | (x >> n);
}
/*---------------------------------------------------*/
