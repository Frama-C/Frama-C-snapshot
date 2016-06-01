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


typedef int T, T4[4], *T_PTR;
const T X, Tab[4];
typedef T_PTR T_PTR_T4[4];
const T_PTR_T4  Tab_Ptr = { &X, &X, &X, &X};


/*@ axiomatic useless_logic_cast {
  @ logic integer v2 = (int)1 + (T)1;
  @ logic int vX = (int) X;
  @ logic int[4] vTab = (T4) Tab;
  @ logic T_PTR_T4 * vTab_Ptr = (T_PTR_T4 *)(&Tab_Ptr);
  @ } */
