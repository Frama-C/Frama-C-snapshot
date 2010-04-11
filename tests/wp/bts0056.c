int T[10];

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// loop assigns not translatable to C lval

/*@ assigns  T[0..4];
    ensures T[5] == \old(T[5]);
*/
void assign_T (void) {
  int i;
  /*@ loop assigns i, T[0..4];
    @ loop invariant 0 <= i;
    */
  for (i = 0; i < 5; i++) {
    T[i] ++;
  }
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
struct Ts { int a; int t[10]; };
struct Ts S;

//@ ensures (T[5] == \old(T[5])) && (S.a == \old(S.a));
void assign_S (void) {
  int i;
  //@ loop assigns i, S;
  for (i = 0; i < 5; i++) {
    S.t[i] = T[i];
  }
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

int main (void) {return 0;}
