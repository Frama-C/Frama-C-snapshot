/* run.config
DONTRUN: main configuration in @PTEST_DIR@/multi_axiomatic_1.i
*/
/*@
	axiomatic ax { logic int Acc(int m); }
	predicate Bnd(integer n,int m) = Acc(m)<=9;
	predicate Bnd(integer n) = Bnd(n, (int) 0);
*/

/*@
	requires Bnd(1);
	ensures \true;
*/
void foo(void) {}

