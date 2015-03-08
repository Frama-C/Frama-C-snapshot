/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -check -load-module @PTEST_DIR@/@PTEST_NAME@
*/

struct S { int i; };

/*@ lemma foo: \forall struct S x; x.i >= 0 || x.i < 0; */

/*@ ensures \result >= x.i; */
int main(struct S x) {
  int y = x.i;
  /*@ assert y == x.i; */
  return y;
}
