/* run.config*
   OPT: -eva @EVA_CONFIG@ -warn-special-float non-finite
   OPT: -eva @EVA_CONFIG@ -warn-special-float nan
   OPT: -eva @EVA_CONFIG@ -warn-special-float none
*/

union { long long l ; float f ; double d ; } u1, u2;
float f;
double d, big;
unsigned long long ull;
double fd(void);
volatile rand;

void main1 (long long l){
  u1.l = l;
  f = u1.f + 1.0;
  u2.l = l;
  d = u2.d + 1.0;
  Frama_C_dump_each();
  
  float vf = fd();
  double vd = fd();
  long long i = vd;
  long long j = vf;
  vd = fd();
  double mvd = -vd / 4.;

  big = 0x1.8p63;
  ull = big;
}


/* Tests the emission of is_nan and is_finite alarms depending on the
   -warn-special-float parameter, and the evaluation and reduction by
   ¬\is_NaN assertions. */
void main2 () {
  float f = rand ? -3. : 17.;
  float f_infinity = rand ? f : 1. / 0.;;
  float f_nan = rand ? f : 0. / 0.;;
  float f_infinity_nan = rand ? f_nan : f_infinity;
  /*@ assert ¬\is_NaN(f); */
  /*@ assert ¬\is_NaN(f_infinity); */
  /*@ assert ¬\is_NaN(f_nan); */
  /*@ assert ¬\is_NaN(f_infinity_nan); */
  if (rand) {
    float infinity = 1. / 0.;
    /*@ assert ¬\is_NaN(infinity); */
  }
  if (rand) {
    float nan = 0. / 0.;
    /*@ assert ¬\is_NaN(nan); */
  }
}

/* Tests the backward propagation of comparisons with NaN.
   When -warn-special-float is none, the result must include NaN. */
double fabs(double x) {
  if (x < 0) return -x;
  else if (x == 0) return 0.0;
  else return x;
}

void main3 () {
  double d = fd();
  double r = fabs(d);
}


void main (long long l) {
  main1(l);
  main2();
  main3();
}
