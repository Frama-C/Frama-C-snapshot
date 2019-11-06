/* run.config_qualif
   CMD: @frama-c@ -wp-share ./share -wp-msg-key no-cache-info,success-only -wp-par 1 -wp-session @PTEST_DIR@/oracle@PTEST_CONFIG@/@PTEST_NAME@.@PTEST_NUMBER@.session -wp-cache offline
   OPT: -eva -eva-msg-key=-summary -then -wp -then -no-eva -warn-unsigned-overflow -wp
 */

/* run.config
   DONTRUN:
*/

int main(int i) { return 1+i; }
