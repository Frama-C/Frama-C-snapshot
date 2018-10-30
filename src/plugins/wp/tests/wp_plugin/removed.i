/* run.config_qualif
   CMD: @frama-c@ -wp-share ./share -wp-msg-key no-time-info,no-step-info -wp-par 1 -wp-timeout 100 -wp-steps 500
   OPT: -eva -then -wp -then -no-eva -warn-unsigned-overflow -wp
 */

/* run.config
   DONTRUN:
*/

int main(int i) { return 1+i; }
