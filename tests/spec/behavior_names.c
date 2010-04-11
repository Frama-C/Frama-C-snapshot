/*@
        behavior foo: ensures \true;
        behavior bar:

        complete behaviors foo, bar, UNEXISTENT_BEHAVIOR;
*/
void f() {
}

/*@ predicate should_be_rejected = \true; */
/*@ predicate should_be_kept = \true; */

/*@
        behavior foo: ensures \true;
        behavior bar:

        disjoint behaviors foo, bar, UNEXISTENT_BEHAVIOR;
*/
void g() {
  /*@ behavior foo: ensures \true; */
  {
    /*@ for foo: assert should_be_kept; */
    f();
    /*@ for foo: assert should_be_kept; */
    f();
  }
  /*@ for foo: assert should_be_rejected; */
  return;
}
