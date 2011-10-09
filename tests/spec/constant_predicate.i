int x;

/*@ predicate P{A} = x == 42; */

/*@ logic integer f{B} = x + 42; */

/*@ lemma foo{C}: P ==> f == 84; */

/*@ ensures f == 84; */
void g () {

  x = 42;
  /*@ assert P; */

}
