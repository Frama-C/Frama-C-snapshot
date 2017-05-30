/* run.config
   OPT: -load-script tests/misc/bts1347.ml -val-show-progress -then -report
*/

int f(int *x) { return *x; }
int g(int *x) { return *(x++); }
