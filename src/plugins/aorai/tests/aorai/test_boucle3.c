/* run.config*
   OPT: -aorai-ltl tests/aorai/test_boucle3.ltl -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@ @PROVE_OPTIONS@
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

int main(){

  /*@ loop invariant 0<=rr<=50;
   */
  while (rr<50) {
    opa();
  }

  opb();

  rr=0;
  while (rr<50) {
    opa();
  }

  return 1;
}
