/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-ltl tests/aorai/goto.ltl -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/

int status=0;
int rr=1;
//@ global invariant inv : 0<=rr<=5000;

/*@ requires rr<5000;
  @ behavior j :
  @  ensures rr<5001;
*/
void opa() {
  rr++;
}

void opb () {
  status=1;
}

void opc () {
  rr=60000;
}

int main(){

  if (rr<5000) goto L;
  opc();

 L4:
  goto L5;

 L:
  opa();
  goto L2;
  opc();

 L6:
  return 1;

 L3:
  goto L4;

  opc();
  goto L2;

 L2 :
  goto L3;


 L5:
  opb();
  goto L6;
}
