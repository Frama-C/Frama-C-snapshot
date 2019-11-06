/* run.config
   OPT:
*/

/* run.config_qualif
   OPT: -wp-prover alt-ergo                                            -wp-prop=-ko  -wp-timeout 100 -wp-steps 1500
   OPT: -wp-prover native:alt-ergo -wp-report=tests/native.report      -wp-prop=-ko  -wp-timeout 100 -wp-steps 1500
   OPT: -wp-prover alt-ergo                                            -wp-prop=ko   -wp-timeout 1 -wp-steps 50
   OPT: -wp-prover native:alt-ergo -wp-report=tests/native.report      -wp-prop=ko   -wp-timeout 1
*/

// --------------------------------------------------------------------------
// --- Absolute
// --------------------------------------------------------------------------

//@ lemma abs_pos: \forall real x,y ; 0 <= x < y ==> \abs(x) < \abs(y) ;
//@ lemma abs_neg: \forall real x,y ; x < y <= 0 ==> \abs(x) > \abs(y) ;

// --------------------------------------------------------------------------
// --- Min/Max
// --------------------------------------------------------------------------

//@ lemma min_inf: \forall real x,y; \min(x,y) <= x && \min(x,y) <= y ;
//@ lemma min_or: \forall real x,y; \min(x,y) == x || \min(x,y) == y ;

//@ lemma max_inf: \forall real x,y; x <= \max(x,y) && y <= \max(x,y) ;
//@ lemma max_or: \forall real x,y; \max(x,y) == x || \max(x,y) == y ;

//@ lemma min_ac: \forall real x,y,z; \min(x,\min(y,z)) == \min(\min(x,y),z) && \min(x,y) == \min(y,x) ;
//@ lemma max_ac: \forall real x,y,z; \max(x,\max(y,z)) == \max(\max(x,y),z) && \max(x,y) == \max(y,x) ;

// --------------------------------------------------------------------------
// --- Square
// --------------------------------------------------------------------------

//@ lemma sqrt_pos: \forall real x,y; 0 <= x ==> 0 <= \sqrt(x) ;
//@ lemma sqrt_mono: \forall real x,y; 0 <= x < y ==> \sqrt(x) < \sqrt(y) ;

// --------------------------------------------------------------------------
// --- Exponential
// --------------------------------------------------------------------------

//@ lemma exp_pos: \forall real x; \exp(x) > 0 ;
//@ lemma log_exp_mul_add: \forall real a,b; \log(\exp(a) * \exp(b)) == a+b ;
//@ lemma exp_log_add_mul: \forall real a,b; a > 0 ==> b > 0 ==> \exp(\log(a) + \log(b)) == a*b ;
//@ lemma pow_2: \forall real a; a>0 ==> \pow(a,2) == a * a ;

// --------------------------------------------------------------------------
// --- Trigonometry
// --------------------------------------------------------------------------

//@ lemma atan_sin_cos: \forall real x; \sin(\atan(x)) / \cos(\atan(x)) == x ;

// --------------------------------------------------------------------------
// --- Hyperbolic
// --------------------------------------------------------------------------

//@ lemma sinh_opp: \forall real x; \sinh(-x) == -\sinh(x) ;
//@ lemma cosh_opp: \forall real x; \cosh(-x) == \cosh(x) ;
//@ lemma tanh_opp: \forall real x; \tanh(-x) == -\tanh(x) ;

// --------------------------------------------------------------------------
// --- Polar
// --------------------------------------------------------------------------

//@ lemma distance: \forall real x,y; \hypot(x,y) == \sqrt( x*x + y*y ); 

// --------------------------------------------------------------------------

/*@ ensures sin_asin:          \forall real x; -1 <= x <= 1   ==> \sin(\asin(x)) == x ;
  @ ensures sin_asin_in_range: \forall real y; \let x = \cos(y) ; \sin(\asin(x)) == x ;
//@ ensures asin_sin:    \forall real x; -\pi/2 <= x <= \pi/2 ==> \asin(\sin(x)) == x ; // TODO: uncomments

  @ ensures cos_acos:          \forall real x; -1 <= x <= 1   ==> \cos(\acos(x)) == x ;
  @ ensures cos_acos_in_range: \forall real y; \let x = \sin(y) ; \cos(\acos(x)) == x ;
//@ ensures acos_cos:          \forall real x; 0 <= x <= \pi  ==> \acos(\cos(x)) == x ; // TODO: uncomments

  @ ensures tan_atan: \forall real x; \tan(\atan(x)) == x ;
//@ ensures atan_tan: \forall real x; -\pi/2 <= x <= \pi/2 ==> \atan(\tan(x)) == x ; // TODO: uncomments

  @ ensures log_pow: \forall real x,b; 0 < x ==> \log(\pow(x,b)) == \log(x)*b ;
  @ ensures log_exp: \forall real x; \log(\exp(x)) == x ;
  @ ensures exp_log: \forall real x; 0 < x ==> \exp(\log(x)) == x ;

  @ ensures min_plus_distrib: \forall real x,y,z; \min(x,y)+z == \min(x+z,y+z) ;

  @ ensures sqrt_pos:  \forall real x; x>0 ==> \sqrt(x)>0 ;
  @ ensures sqrt_pos0: \forall real x; x>=0 ==> \sqrt(x)>=0 ;
*/
void ok(void) { };

/*@ ensures ko: sin_asin: \forall real x; \sin(\asin(x)) == x ;
  @ ensures ko: cos_acos: \forall real x; \cos(\acos(x)) == x ;
  @ ensures ko: asin_sin: \forall real x; \asin(\sin(x)) == x ;
  @ ensures ko: acos_cos: \forall real x; \acos(\cos(x)) == x ;
  @ ensures ko: atan_tan: \forall real x; \atan(\tan(x)) == x ;
  @ ensures ko: log_pow: \forall real x,b; \log(\pow(x,b)) == \log(x)*b ;
  @ ensures ko: exp_log: \forall real x; \exp(\log(x)) == x ;
  @ ensures ko: exp_log_add_mul: \forall real a,b; \exp(\log(a) + \log(b)) == a*b ;
  @ ensures ko: sqrt_pos: \forall real x; \sqrt(x) >= 0 ;
*/
void ko(void) { };
