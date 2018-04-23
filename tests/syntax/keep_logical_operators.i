/* run.config 
   OPT:-print -keep-logical-operators
*/
int test(int a, int b, int c) {

  if (a && (b || c)) {
    return 1;
  }
  return 2;

}
