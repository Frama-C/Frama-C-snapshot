/* run.config
   DONTRUN:
 */

/* run.config_qualif
   OPT: -wp-timeout 1
   OPT: -wp-prover native:alt-ergo -wp-timeout 1
   OPT: -wp-prover native:coq
 */

/*@ axiomatic maps {
      type model_digit = octet | sextet;
      logic integer foo(model_digit i);
    }
*/

int foo()
{
  // Shall not fail (parse error in BTS issue)
  //@assert ko: \forall int i; i == foo(octet);
  return 0;
}
