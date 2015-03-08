/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/test_struct.ya -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/

struct People{
	int Age;

	char Gender;

};

struct People nobody;

int myAge=0;

void increment(){
    nobody.Age++;
    myAge++;
}


int main() {
    nobody.Age=0;
    increment();
    return 0;
}
