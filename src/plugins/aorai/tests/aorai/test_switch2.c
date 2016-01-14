/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-ltl tests/aorai/test_switch2.ltl -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/

int status=0;
int rr=1;
//@ global invariant inv : 0<=rr<=50;

/*@ requires rr<50;
  @ behavior j :
  @  ensures rr<51;
*/
void opa() {
  rr++;
}

void opb () {
  status=1;
}

void opc() {
  rr=9000;
}

int main(){

  switch (rr) {
  case 1 : 
    opa();
    break;
  case 3 : 
    opa();
  default : 
    opc();
  }
  opb();

  return 1;
}
