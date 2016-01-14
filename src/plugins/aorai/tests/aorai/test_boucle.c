/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-ltl tests/aorai/test_boucle.ltl -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/

/*@ requires \true;
  @ ensures 0<=\result<=1;
*/
int a() {
  return 1;
}

/*@ requires \true;
  @ ensures 1<=\result<=2;
*/
int b() {
  call_to_an_undefined_function(); 
  return 2;
}

/*@ requires \true;
  @ ensures 0<=\result<=1;
*/
int main(){
  int x=a();
  /*@ loop invariant i : 
    @      0<=x<=11;
   */
  while (x<10) {
    x+=b();
  }
  return a();
}
