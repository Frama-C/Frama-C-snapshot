/* run.config
   OPT: -wp-invariants -wp-model Hoare
*/

/* run.config_qualif
OPT: -journal-disable -wp -wp-invariants -wp-model Typed+ref -wp-par 1 -wp-prop="-qed_ko"
OPT: -journal-disable -wp -wp-invariants -wp-model Typed+ref -wp-par 1 -wp-prop qed_ko -wp-timeout 2
*/

/* run.config_qed
DONTRUN: (config_qed) see config_qualif
*/

//@ensures qed_ok: \result == 5;
int simple_inv (void) {
  int i = 0;
  while (i < 5) {
    //@ invariant qed_ok: 0 <= i < 5 ;
    i++;
  }
  return i;
}
int inv_from_init (void) {
  int x = 5;
  int i = 0;
  //@ loop assigns qed_ok: i ;
  while (i < 5) {
    //@ invariant qed_ok: I: i < x ;
    i++;
  }
  return i;
}

int caveat_inv (int n) {
  int i, s = 0;
  //@ loop assigns qed_ok: i, s;
  for (i = 0; i < n; i++) {
    //@ invariant qed_ok: 0 <= i < n ;
    s++;
  }
  return s;
}

int double_loop (void) {
  for (int i = 0; i < 10; i++) {
    //@ invariant qed_ok: 0 <= i < 10 ;
    //@ loop assigns qed_ok: j;
    for (int j = 0; j < i; j++) {
      //@ invariant qed_ok: 0 <= j < i ;
      ;
    }
  }
  return 0;
}

int T2[10][20];

/*@ ensures qed_ok: post: 
    \forall int ii, jj; 0 <= ii < 10 ==> 0 <= jj < 20 ==> T2[ii][jj] == 0;
*/
void razT2simple (void) {
  for (int i = 0; i < 10; i++) {
    /*@ invariant qed_ok: I1: \forall int ki, j; 0 <= ki < i ==> 0 <= j < 20 
                   ==>T2[ki][j] == 0;
    */

    //@ loop assigns j, T2[i][0..19];
    for (int j = 0; j < 20; j++) {
    /*@ invariant qed_ok: I2: 
           (\forall int ki, kj; 0 <= ki < i ==> 0 <= kj < 20 
                   ==> T2[ki][kj] == 0)
        && (\forall int kj; 0 <= kj < j ==> T2[i][kj] == 0); */
      T2[i][j] = 0;
    }
  }
}

/*@ ensures post: 
    \forall int i, j; 0 <= i < 10 ==> 0 <= j < 20 ==> T2[i][j] == 0;
*/
void razT2 (void) {
  for (int i = 0; i < 10; i++) {
    /*@ invariant Ii: \forall int ki, j; 0 <= ki < i ==> 0 <= j < 20 
                   ==>T2[ki][j] == 0;
    */
    //@ loop assigns j, T2[i][0..19];
    for (int j = 0; j < 20; j++) {
    /*@ invariant \forall int ki, kj; 0 <= ki < i ==> 0 <= kj < 20 
                   ==> T2[ki][kj] == 0; */
    /*@ invariant \forall int kj; 0 <= kj < j ==> T2[i][kj] == 0; */
      T2[i][j] = 0;
    }
  }
}

//@ requires c > 0;
int bts618 (int c) {
  int x = 1;
 L : ;
  x++;
      //@ invariant qed_ok: (0 < c <= \at(c, Pre)) && x == 2 + (\at(c, Pre) - c);
  if (--c > 0) goto L;
  return x;
}

int T[10];

/*

int both_inv_and_loop_inv (int n) {

  //@ loop invariant \forall int k; 0 <= k && k < i ==> T[k] == 0; 
  for (int i = 0; i < 10; i++)
    T[i] = 0;
}
*/

// Add this test for [new_loops] only
/*@ behavior n_neg : 
      assumes n < 0;
      ensures \result == 10;
    behavior n_pos :
      assumes n >= 0;
      ensures \result == 21;
 */
int non_natural_loop (int n) {
  int x = 1;
  if (n < 0) {
    x = 0;
    n = 10;
    }
  else {
    n = 20;
    L : x = x + 2;
    }
  if (x < n) {
    //@ for n_neg: invariant x < n && n == 10 && x%2 == 0;
    ;
    //@ for n_pos: invariant qed_ok: n == 20;
    ;
    goto L;
  }
  return x;
}

//@ requires 0 <= n; ensures qed_ok: 0 <= \result < n + 5;
int test_for_tag (int n) {
  int i = 0;
  //@ loop assigns qed_ok: i;
  while (i < n) {
    i += 2;
    //@ invariant qed_ok: 2 <= i < n+2;
    i += 3;
  }
  return i;
}

//@ ensures \old(T[0]) == 0 ==> T[0] == 0;
int double_entry (int n) {
  int i = 0;

  if (T[0] == 0) goto L;

  while (i < n) {
    if (T[i] == 0)
      T[i] ++;

L: 
    if (T[i] > 5)
      break;

    i++;
  }
  return i;
}

/*@ 
 behavior without_inv :
  ensures qed_ko : \result == 6; 
 behavior qed_ok :
  ensures qed_ok: ok : \result == 6; 
*/
int goto_natural_loop (int c) {
  int c = 0;
L : if (c > 5) goto R;
    //@ for qed_ok: invariant qed_ok: c <= 5;
    c++;
    goto L;
R : return c;
}

