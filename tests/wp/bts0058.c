int T[10];

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// loop assigns can specify a loop invariant

//@ ensures T[5] == \old(T[5]);
void assign_T (void) {
  int i;
  //@ loop assigns i, T[0..i-1];
  for (i = 0; i < 5; i++) {
    T[i] ++;
  }
  //@ assert i==5;
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void assign_to_inv (void) {
  int i;
  /*@ loop invariant \forall int j; j > i-1 ==> T[j] == \at(T[j], Pre);
    @ loop assigns i, T[0..4];
    */
  for (i = 0; i < 5; i++) {
    T[i] ++;
  }
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
