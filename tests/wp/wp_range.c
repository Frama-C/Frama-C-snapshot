/* run.config_phoare
   OPT:  -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

// le tout passe avec z3
// frama-c-gui -wp-mm 2 -wp-prover z3 wp_range.c &


int T[10];

/*@ requires 0 <= i < 10;
  @ ensures T[i] == \old(T[i]) + 1;
  */
void incr_elem_tab (int i) {
  T[i]++;
}

/*@ requires \valid_range(p,0,i) ;
  ensures *(\old(p)+i) == 78;
*/
void main (int *p,int i) {
  p++;
  p++;
  p++;
  *(p+i-3) = 78;
}

/*@ requires 0 <= i < 5;
  @ ensures \result == i;
  */
int local_tab (int i) {
  int t[5];
  //@ assert \valid (t+i);
  t[i] = i;
  return t[i];
}

int * P;

//@ ensures \forall int j; n <= j ==> *(P + j) == \old(*(P + j));
int loop_assign (int n) {
  int i;
  //@ loop assigns i, P[0..(n-1)];
  for (i = 0; i < n; P++, i++) 
    *P = 0;
  return i;
}
