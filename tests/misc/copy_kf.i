/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@
*/

/*@ requires \valid(p); assigns *p; ensures *p == x; */
void g(int* p, int x);

/*@ requires 0 <= x <= 10;
    ensures \result == 2 * x;
*/
int f(int x) { int y; g(&y,x); return x + y; }
