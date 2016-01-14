/* run.config
   EXECNOW: make -s tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/bts1289.ya -load-module tests/aorai/Aorai_test.cmxs -aorai-test 1 -aorai-test-number @PTEST_NUMBER@
   OPT: -aorai-automata tests/aorai/bts1289-2.ya -load-module tests/aorai/Aorai_test.cmxs -aorai-test 1 -aorai-test-number @PTEST_NUMBER@
 */

void a(void) {}

void main(void)
{
	//@ loop assigns i;
	for (int i=0; i<10; ++i)
		a();
}

