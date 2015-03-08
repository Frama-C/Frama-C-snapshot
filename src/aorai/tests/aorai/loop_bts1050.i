/* run.config
   EXECNOW: make -s tests/aorai/Aorai_test.cmxs
   OPT: -aorai-automata tests/aorai/loop_bts1050.ya -aorai-test 1 -load-module tests/aorai/Aorai_test.cmxs -aorai-acceptance -aorai-test-number @PTEST_NUMBER@

*/
void f(){};

void g(){};

int main(int c){
    if (c<0){ c = 0;}
    if (c>0){ c = 5;}
    while (c){
        f();
        g();
        c--;
    }
    return 0;
}
