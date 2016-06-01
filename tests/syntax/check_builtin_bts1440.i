/* run.config
STDOPT: +"-machdep gcc_x86_32 -kernel-debug 1 -kernel-msg-key file -kernel-msg-key=-file:transformation"
*/

/*@ ensures \result >= i;
  @ ensures \result >= j;
  @ ensures \result == i || \result == j;
  @*/
int max(int i, int j) {
  return (i>=j) ? i : j;
}
