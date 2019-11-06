/* run.config_qualif
   OPT: -wp-prover why3:alt-ergo
   OPT: -wp-model real
 */

/*@ lemma test_float_compare:
      \forall float x,y;
      \is_finite(x) && \is_finite(y) ==>
      \le_float(x,y) ==> \lt_float(x,y) || \eq_float(x,y);
*/

/*@ lemma test_double_compare:
      \forall double x,y;
      \is_finite(x) && \is_finite(y) ==> \le_double(x,y) ==>
         \lt_double(x,y) || \eq_double(x,y);
*/

/*@ lemma test_float_compare_greater:
      \forall float x,y;
      \is_finite(x) && \is_finite(y) ==>
      \ge_float(x,y) ==> \gt_float(x,y) || \eq_float(x,y);
*/

/*@ lemma test_double_compare_greater:
      \forall double x,y;
      \is_finite(x) && \is_finite(y) ==> \ge_double(x,y) ==>
         \gt_double(x,y) || \eq_double(x,y);
*/

/*@ lemma finite_32_64:
      \forall float x;
      \is_finite(x) ==> \is_finite((double)x);
*/

/*@ lemma finite_32_64_real:
      \forall float x;
      \is_finite(x) ==> ((real) x) == ((real)(double) x) ;
*/

/*@
  requires \is_finite(a) && \is_finite(b);
  ensures DEF:  \result == ((a < b) ? 1 : 0) ;
  ensures REL1: \result <==> a < b ;
  ensures REL2: \result <==> a < b ;
*/
int cmp_ff(float a,float b) { return a < b; }

/*@
  requires \is_finite(a) && \is_finite(b);
  ensures DEF:  \result == ((a < b) ? 1 : 0) ;
  ensures REL1: \result <==> a < b ;
  ensures REL2: \result <==> a < b ;
*/
int cmp_dd(double a,double b) { return a < b; }

/*@
  requires \is_finite(a) && \is_finite(b);
  ensures DEF:  \result == ((a < b) ? 1 : 0) ;
  ensures REL1: \result <==> a < b ;
  ensures REL2: \result <==> a < b ;
*/
int cmp_fd(float a,double b) {
  //@ assert \is_finite((double)a);
  //@ assert ((real) a) == ((real)(double)a) ;
  return a < b;
}

/*@
  ensures POS:  \lt_float(a,b) <==> \result == 1 ;
  ensures NEG: !\lt_float(a,b) <==> \result == 0 ;
*/
int cmp_fnan(float a,float b) { return a < b; }
