/*@     behavior foo: ensures \true;

        behavior bar:

        complete behaviors foo, bar, UNEXISTENT_BEHAVIOR;
*/
void f() {
}

/*@ predicate should_be_rejected = \true; */
/*@ predicate should_be_kept = \true; */

/*@     behavior foo: ensures \true;

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

/*@ behavior foo: ensures \true;
  behavior foo: ensures should_be_rejected;
 */
void h () { }

/*@ behavior foo: ensures \true; */
void i () {
  //@ behavior foo: ensures should_be_rejected;
  ;
}

void j () {
  int x = 0;

  //@ behavior foo: ensures \true;
  { x++;
    //@ behavior foo: ensures should_be_rejected;
    if (x)
     { //@ behavior bar: ensures \true;
       x++;
     }
    else {
      //@ behavior bar: ensures should_be_kept;
      x++;
    }
  }
}

/*@ behavior boolean: ensures boolean:\true; 
    behavior char: ensures char:\true; 
    behavior for: ensures for:\true; 
    behavior while: ensures while:\true; 
    behavior ensures: ensures ensures: \true; 
    behavior logic: ensures logic: \true; 
 */
void keyword_as_behavior_and_term_names () {
  ;
}
