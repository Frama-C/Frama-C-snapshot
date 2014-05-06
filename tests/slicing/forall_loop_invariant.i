/* run.config
   OPT: -slice-assert main -then-on 'Slicing export' -print -then-on default -slice-value t -then-on 'Slicing export 2' -print
 **/


/* cf discussion on bts 690 */

int t[10], u[10];
int main(void) {
  /*@ loop invariant 0 <= i <= 10;
    @ loop invariant \forall int k; 0 <= k < i ==> t[k] == 1;
    @ loop invariant \forall int k; 0 <= k < i ==> u[k] == 2;
    @ */
  for(int i = 0; i < 10; i++) {
    t[i] = 1;
    u[i] = 2;
  }
  /*@ assert t[2] == 1; */
}
