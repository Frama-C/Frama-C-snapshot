/* run.config
   OPT: -wp-no-print -wp-rte
*/

/* run.config_qualif
   OPT: -wp-prover=why3:alt-ergo -wp-prop=-lack -wp-timeout 90 -then -wp-rte -wp -wp-prop=-lack
*/

// The use '-wp-prover=z3,why3:alt-ergo' gives better results.

typedef unsigned uint32_t ;
typedef unsigned long long uint64_t ;

/*@ axiomatic mult {
  @ lemma sizeof_ok: ok: sizeof(uint64_t) == 2*sizeof(uint32_t);

  @ lemma ax1: lack: \forall integer x, y; 0<x  && 0<y ==> 0 <= 2*x*(y/2) <= x*y;

  @ lemma ax2: lack: \forall integer x, y; (uint64_t)(x * ((uint64_t)y)) == (uint64_t)(x*y) ;
  @ lemma ax3: lack: \forall integer x, y; (uint64_t)(x + ((uint64_t)y)) == (uint64_t)(x+y) ;
  @ lemma ax4:   ok: \forall integer x, y; (uint64_t)(((uint64_t)x) * y) == (uint64_t)(x*y) ;
  @ lemma ax5:   ok: \forall integer x, y; (uint64_t)(((uint64_t)x) + y) == (uint64_t)(x+y) ;

  @ lemma ax7:   ok: \forall integer x, y; 0<=x && 0<=y && ((y%2) > 0) ==> 2*x*(y/2) + x == x*y;

  @ }
  @ */

//@ ensures product: \result == a*b;
uint64_t BinaryMultiplication (uint32_t a, uint32_t b) {
  //@ assert a1: ok: deductible: a*b == (uint64_t)(a*b); // deductible from size of C types
  uint64_t r=0;
  uint64_t x=a;
  if (b != 0) {
    /*@ loop assigns r, x, b;
      @ loop invariant inv1: ok: r+x*b == \at(a*b, LoopEntry);
      @ loop invariant inv2: ok: b > 0;
      @ loop variant ok: b ;
      @*/
    while (1) {
      //@ assert a2: ok: b>1 ==> 2*x == (uint64_t)(2*x);
      //@ assert a3: ok: x*b == (uint64_t)(x*b);
      //@ assert a4: ok: ((b%2) != 0) ==> 2*x*(b/2) + x == x*b;
      //@ assert a5: ok: ((b%2) == 0) ==> 2*x*(b/2)     == x*b;
      if (b%2) r=r+x;
      //@ assert a6: lack: ok_z3: r+2*x*(b/2) == \at(a*b, Pre);
      b=b/2;
      if (b==0) break;
      x=x*2;
    };
  }
  return r;
}
