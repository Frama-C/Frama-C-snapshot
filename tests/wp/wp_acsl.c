/*  run.config
OPT: -wp-builtin-why-file -wp-debug 2
DONTRUN:
OPT: -wp-mm 0 -wp-debug 1 -journal-disable -wp-no-proof
*/
/* run.config_dev
OPT: -wp-mm 0 -wp-debug 2 -journal-disable -wp-proof
*/

// Be carreful : this is not the real definition (TODO : change that)
int * NULL = (int*) 0;
      //@ global invariant null_inv : NULL == \null;

int G, X, T[10];

void tnull () {
  int * p;
  p = &G;
  //@ assert (p != \null) ;
  p = NULL;
  //@ assert (p == \null) ;
}

// @ ensures \result == \min(x, y); TODO : at the moment \min has a wrong type
int min (int x, int y) {
  return x < y ? x : y;
}

/*@ ensures \separated (\result, &X); */
int * ptrX (int * p) {
  p = &G;
  return p;
}

/*@ requires 0 < n;
    ensures \result <= n;
*/
int loop (int n) {
  int i, s = 0;
  /*@ loop invariant s <= i && 0<= i <= n;
      loop assigns i, s;
      loop variant (n - i);
  */
  for (i = 0; i < n; i++) {
    if (T[i] == 0) s++;
  }

  return s;
}

//@ ensures \result <= n;
int stmt_contract (int n) {
  int i, s = 0;
  /*@ ensures s <= n;
      assigns i, s;
  */
  //@ loop invariant s <= i && 0<= i <= n;
  for (i = 0; i < n; i++) {
    if (T[i] == 0) s++;
  }

  return s;
}
