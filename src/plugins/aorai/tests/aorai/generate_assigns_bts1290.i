/* run.config*
   OPT: -aorai-automata tests/aorai/generate_assigns_bts1290.ya -load-module tests/aorai/Aorai_test.cmxs -aorai-test 1 -aorai-test-number @PTEST_NUMBER@ @PROVE_OPTIONS@
 */
void main(void)
{
	//@ loop assigns i;
	for (int i=0; i<10; ++i)
		;
}
