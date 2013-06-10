/* run.config_no_native_dynlink
   CMD: bin/toplevel.byte
   OPT: -load-script tests/misc/bts1347.ml -load-module lib/plugins/Report -then -report
*/
/* run.config
   OPT: -load-script tests/misc/bts1347.ml -load-module lib/plugins/Report -then -report
*/

int f(int *x) { return *x; }
int g(int *x) { return *(x++); }
