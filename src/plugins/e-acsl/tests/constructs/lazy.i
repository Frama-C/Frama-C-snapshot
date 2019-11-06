/* run.config
   COMMENT: terms and predicates using lazy operators
*/

int main(void) {
  int x = 0, y = 1;

  // lazy predicates
  /*@ assert x == 0 && y == 1; */
  /*@ assert ! (x != 0 && y == 1/0); */
  /*@ assert y == 1 || x == 1; */
  /*@ assert x == 0 || y == 1/0; */
  /*@ assert x == 0 ==> y == 1; */
  /*@ assert x == 1 ==> y == 1/0; */
  /*@ assert x ? x : y; */
  /*@ assert y ? y : x; */
  /*@ assert x == 1 ? x == 18 : x == 0; */

  // these predicates are not lazy, but are encoded by lazy ones
  /*@ assert x == 2 <==> y == 3; */
  /*@ assert x == 0 <==> y == 1; */

  // lazy terms
  /*@ assert (x ? x : y) == (x == 0); */
  /*@ assert (x && y) || y; */      // converted into predicate by the kernel
  /*@ assert (x || y) && y == 1; */ // converted into predicate by the kernel
  /*@ assert (x || y) == y; */
  /*@ assert (x && y) == x; */

  return 0;
}
