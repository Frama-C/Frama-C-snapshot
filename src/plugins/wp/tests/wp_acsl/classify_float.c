/* run.config_qualif
   OPT: -wp-prover alt-ergo
   OPT: -wp-prover why3:alt-ergo
   OPT: -wp-prover coq -wp-script tests/wp_acsl/classify_float.script
 */

/*@
  lemma NaN_not_finite: \forall double x; !( \is_NaN(x) && \is_finite(x) );
  lemma InfP_not_finite: \forall double x; !( \is_plus_infinity(x) && \is_finite(x) );
  lemma InfN_not_finite: \forall double x; !( \is_minus_infinity(x) && \is_finite(x) );
 */


#include <math.h>
