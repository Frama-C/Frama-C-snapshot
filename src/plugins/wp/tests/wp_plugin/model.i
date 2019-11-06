/* run.config
   CMD: @frama-c@ -wp-share ./share -wp-msg-key cluster,shell,print-generated -wp-prover why3
   OPT: -wp-model Typed -wp -wp-gen -wp-print -then -wp-model Typed+ref -wp -wp-gen -wp-print
*/

/* run.config_qualif
   OPT: -wp-msg-key cluster -wp-model Typed -wp-check -then -wp -wp-model Typed+ref -wp-check
*/

//@ predicate P(integer a);

//@ ensures P(\result);
int f(int *p,int k) { return p[k]; }
