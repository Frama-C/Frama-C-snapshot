/* run.config_no_native_dynlink
   CMD: bin/toplevel.byte
   OPT: -load-script tests/misc/bts1347.ml -then -report
*/
/* run.config
   OPT: -load-script tests/misc/bts1347.ml -then -report
*/

int f(int *x) { return *x; }
int g(int *x) { return *(x++); }
