/* run.config
   CMD: @frama-c@ -wp-share ./share -wp-log cluster,shell
   OPT: -wp-model Typed -wp -wp-gen -wp-print -then -wp-model Typed+ref -wp -wp-gen -wp-print
*/

/* run.config_qualif
   CMD: @frama-c@ -wp-share ./share -wp-log cluster,shell
   OPT: -wp-model Typed -wp -wp-check -then -wp-model Typed+ref -wp -wp-check
*/

//@ predicate P(integer a);

//@ ensures P(\result);
int f(int *p,int k) { return p[k]; }
