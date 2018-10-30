/* run.config
   OPT:
*/

/* run.config_qualif
   CMD: @frama-c@ -no-autoload-plugins -load-module wp -wp -wp-par 1 -wp-timeout 100 -wp-steps 500 -wp-share ./share -wp-msg-key shell,no-time-info,no-step-info -wp-out @PTEST_FILE@.@PTEST_NUMBER@.out @PTEST_FILE@
   OPT: -wp-prover alt-ergo -wp-report=tests/qualif.report
   OPT: -wp-prover why3:alt-ergo -wp-report=tests/qualif-why3.report
*/

// --------------------------------------------------------------------------
// --- Conversions
// --------------------------------------------------------------------------

//@ lemma floor: \forall real x; \floor(x) <= x < \floor(x)+1 ;
//@ lemma ceil:  \forall real x; \ceil(x)-1 < x <= \ceil(x) ;
