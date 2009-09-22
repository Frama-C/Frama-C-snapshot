/* bts 60: 1 should be lifted to a real number implicitely.  */

/*@ ensures 1.0 == 1; */
void f();

/*@ lemma foo: 1.0 == (float)1; */

/*@ axiomatic toto {
  @ logic integer g;
  @ predicate foo(real x);
  @ } */

void f() {
 double B;
/*@ assert B==g; */
}


/*@ ensures foo(\result); */
int g() { return 0; }
