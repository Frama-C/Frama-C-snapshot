/* run.config
   OPT: -wp-driver tests/wp_plugin/bit_test.driver
 */

/* run.config_qualif
   OPT: -wp-driver tests/wp_plugin/bit_test.driver -wp-prover why3:alt-ergo -wp-check
*/

/*@
axiomatic btest {
  logic 𝔹 lbtest(ℤ v, ℤ n) ;
  predicate btest(ℤ v, ℤ n) ;
  logic 𝔹 lbtest_qed(ℤ v, ℤ n) ;
  predicate btest_qed(ℤ v, ℤ n) ;

  }
 */

/*@
    ensures ko: lbtest(order1, 0) ≡ lbtest(order2, 0);
 */
void check1(int order1, int order2)
{
  return;
}

/*@
    ensures ko: lbtest_qed(order1, 0) ≡ lbtest_qed(order2, 0);
 */
void check2(int order1, int order2)
{
  return;
}


#include "__fc_integer.h"

/*@
    ensures ko: bit_test(order1, 0) ≡ bit_test(order2, 0);
 */
void check3(int order1, int order2)
{
  return;
}
