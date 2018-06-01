/* Test case for gitlab issue #326 */

/*@ axiomatic buggy {
    predicate bla(integer n);
    axiom will_fail: INEXISTENT_SYMBOL == 1;
  }
*/

/*@ requires bla(n);*/
void f(int n);
