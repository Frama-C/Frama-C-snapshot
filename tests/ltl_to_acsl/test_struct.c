/* run.config
   OPT: -ltl-automata tests/ltl_to_acsl/test_struct.ya -ltl-test 1 -ltl-acceptance
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
