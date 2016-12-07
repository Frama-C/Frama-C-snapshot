/* run.config
DONTRUN: does not work yet.
OPT: -print
*/

//@ requires i > 0 && \valid(a+(i-1));
int f(int i, int t[static i], int a[i]) { return t[i-1] + a[i-1]; }
