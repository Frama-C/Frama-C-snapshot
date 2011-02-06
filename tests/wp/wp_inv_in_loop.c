/* run.config_phoare
  OPT:  -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

// cf aussi bts494 examples in wp_loop.c

//@ensures \result == 5;
int simple_inv (void) {
  int i = 0;
  while (i < 5) {
    //@ invariant 0 <= i < 5 ;
    i++;
  }
  return i;
}
int inv_from_init (void) {
  int x = 5;
  int i = 0;
  //@ loop assigns i ;
  while (i < 5) {
    //@ invariant i < x ;
    i++;
  }
  return i;
}
/*
int caveat_inv (int n) {
  int i, s = 0;
  //@ loop assigns i, s;
  for (i = 0; i < n; i++) {
    //@ invariant 0 <= i < n ;
    s++;
  }
  return s;
}
*/
int double_loop (void) {
  for (int i = 0; i < 10; i++) {
    //@ invariant 0 <= i < 10 ;
    //@ loop assigns j;
    for (int j = 0; j < i; j++) {
      //@ invariant 0 <= j < i ;
      ;
    }
  }
  return 0;
}

int T[10][20];
/*@ ensures post: 
    \forall int i, j; 0 <= i < 10 ==> 0 <= j < 20 ==> T[i][j] == 0;
*/
void razT2 (void) {
  for (int i = 0; i < 10; i++) {
    /*@ invariant Ii: \forall int i0, j; 0 <= i0 < i ==> 0 <= j < 20 
                   ==>T[i0][j] == 0;
    */
    //@ loop assigns j, T[i][0..19];
    for (int j = 0; j < 20; j++) {
    /*@ invariant \forall int i0, j0; 0 <= i0 < i ==> 0 <= j0 < 20 
                   ==> T[i0][j0] == 0; */
    /*@ invariant \forall int j0; 0 <= j0 < j ==> T[i][j0] == 0; */
      T[i][j] = 0;
    }
  }
}
