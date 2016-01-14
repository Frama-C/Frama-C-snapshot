/* run.config
   OPT: -wp-driver tests/wp_plugin/abs.driver
 */

/* run.config_qualif
   OPT: -wp -wp-driver tests/wp_plugin/abs.driver -wp-proof alt-ergo
   OPT: -wp -wp-driver tests/wp_plugin/abs.driver -wp-proof coq -wp-script tests/wp_plugin/abs.script
   OPT: -wp -wp-driver tests/wp_plugin/abs.driver -wp-proof why3:alt-ergo
*/

/*@ axiomatic Absolute { logic integer ABS(integer x) ; } */

/*@ ensures \result == ABS(x) ; */
int abs(int x)
{
  if (x < 0) return -x ;
  return x ;
}
