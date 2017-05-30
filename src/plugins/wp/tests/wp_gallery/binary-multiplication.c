/* run.config
   OPT: -wp-no-print -wp-rte
*/

/* run.config_qualif
   OPT: -wp-prop=-lack -then -wp-rte -wp -wp-prop=-lack
*/

typedef unsigned uint32_t ;
typedef unsigned long long uint64_t ;

/*@ axiomatic mult {
  @ lemma sizeof_uint32_t: ok: sizeof(uint32_t) == 4; // 4 bytes: 32 bits
  @ lemma sizeof_uint64_t: ok: sizeof(uint64_t) == 8; // 8 bytes: 64 bits
  @
  @ lemma ax1: lack: \forall integer x, y; 0<=x && 0< y ==> 0 <= x <= x*y;
  @ lemma ax2:   ok: \forall integer x, y; 0<=x && 0<=y ==> 0 <= 2*x*(y/2) <= x*y;
  @ lemma ax3: lack: \forall integer x, y; (uint64_t)(x * ((uint64_t)y)) == (uint64_t)(x*y) ;
  @ lemma ax4: lack: \forall integer x, y; (uint64_t)(x + ((uint64_t)y)) == (uint64_t)(x+y) ;
  @ lemma ax5:   ok: \forall integer x, y; (uint64_t)(((uint64_t)x) * y) == (uint64_t)(x*y) ;
  @ lemma ax6:   ok: \forall integer x, y; (uint64_t)(((uint64_t)x) + y) == (uint64_t)(x+y) ;
  @ }
  @ */

//@ ensures product: \result == a*b;
uint64_t BinaryMultiplication (uint32_t a, uint32_t b) {
  //@ assert a1: ok: deductible: a*b <= 18446744073709551615; // deductible from type size
  uint64_t r=0;
  uint64_t x=a;
  if (b != 0) {
    /*@ loop assigns r, x, b;
      @ loop invariant inv1: ok: r+x*b == \at(a*b, LoopEntry);
      @ loop invariant inv2: ok: r+x == (uint64_t)(r+x);
      @ loop variant ok: b ;
      @*/
    while (1) {
      //@ assert a2: ok: b>1 ==> 2*x == (uint64_t)(2*x);
      //@ assert a3: ok: x*b == (uint64_t)(x*b);
      if (b%2) r=r+x;
      b=b/2;
      if (b==0) break;
      x=x*2;
    };
  }
  return r;
}
