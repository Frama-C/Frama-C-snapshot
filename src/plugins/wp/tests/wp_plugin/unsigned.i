/* run.config
   DONTRUN:
*/

/* run.config_qualif
   OPT: -session tests/wp_plugin/unsigned -wp-prover script
*/

/*@
  lemma U32:
  \forall unsigned int x; (x & ((1 << 32)-1)) == x ;
 */
