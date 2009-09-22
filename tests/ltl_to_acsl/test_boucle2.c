/* run.config
   OPT: -ltl tests/ltl_to_acsl/test_boucle2.ltl -ltl-test 1 -ltl-acceptance
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
  //@ ghost int tmp = 1;

  //@ ghost tmp=0;

  return 1;
}
