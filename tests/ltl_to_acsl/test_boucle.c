/* run.config
   OPT: -ltl tests/ltl_to_acsl/test_boucle.ltl -ltl-test 1 -ltl-acceptance
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
