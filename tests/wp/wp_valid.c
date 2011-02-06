// see also : tests/wp/bts0085.c for a lot of tests about valid pointers

int * P;
int T[10];

//@ requires \valid(P);
void f (void) {
  int x; //@ assert P != &x;
}

// If we don't know that \valid(p), we sould not be able to proof this.
//@ ensures \result == \old(*p);
int disj (int * p) {
  int x = 3;
  return *p;
}

/*@ ensures ok1: 0 <= i < 10 ==> \valid (\result);
    ensures ko: i == 10 ==> \valid (\result);
 */
int * valid_in_global_array (int i) {
  return T+i;
}
