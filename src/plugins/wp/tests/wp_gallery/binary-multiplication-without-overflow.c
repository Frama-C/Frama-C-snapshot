/* run.config
   OPT: -warn-unsigned-overflow -wp-no-print -wp-rte
*/

/* run.config_qualif
   OPT: -warn-unsigned-overflow -wp-prop=-lack -then -warn-unsigned-overflow -wp-rte -wp -wp-prop=-lack
*/

typedef unsigned uint32_t ;
typedef unsigned long long uint64_t ;

/*@ axiomatic mult {
  @ lemma sizeof_uint32_t: ok: sizeof(uint32_t) == 4; // 4 bytes: 32 bits
  @ lemma sizeof_uint64_t: ok: sizeof(uint64_t) == 8; // 8 bytes: 64 bits
  @
  @ lemma ax1: lack: \forall integer x, y; 0<=x && 0< y ==> 0 <= x <= x*y;
  @ lemma ax2:   ok: \forall integer x, y; 0<=x && 0<=y ==> 0 <= 2*x*(y/2) <= x*y;
  @ }
  @ */

//@ ensures product: \result == a*b;
uint64_t BinaryMultiplication (uint32_t a, uint32_t b) {
  //@ assert a1: ok: deductible: a*b <= 18446744073709551615; // deductible from type size
  uint64_t r=0;
  uint64_t x=a;
  if (b != 0) {
    /*@ loop assigns ok: r, x, b;
      @ loop invariant inv1: ok: r+x*b == \at(a*b, LoopEntry);
      @ loop invariant inv2: ok: deductible: 2*x*(b/2) <= 18446744073709551615; // deductible from inv1, ax2, ax1, a1 and  x>=0, b>0, r>=0 
      @ loop variant ok: b ;
      @*/
    while (1) {
      if (b%2) r=r+x;
      b=b/2;
      if (b==0) break;
      x=x*2;
    };
  }
  return r;
}
