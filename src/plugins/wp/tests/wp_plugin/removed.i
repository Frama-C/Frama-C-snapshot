/* run.config_qualif
   CMD: @frama-c@ -wp-share ./share -wp-msg-key no-time-info,no-step-info
   OPT: -val -then -wp -then -no-val -warn-unsigned-overflow -wp
 */

/* run.config
   DONTRUN:
*/

int main(int i) { return 1+i; }
