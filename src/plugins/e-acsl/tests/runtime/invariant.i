/* run.config
   COMMENT: invariant
   STDOPT: +"-slevel 11"
*/

int main(void) {
  int x = 0;
  for(int i = 0; i < 10; i++) {
    /*@ invariant 0 <= i < 10; */
    x += i;
    /*@ invariant i <= x; */
  }
  return 0;
}
