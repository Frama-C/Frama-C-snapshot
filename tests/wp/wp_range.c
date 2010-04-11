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
  t[i] = i;
  return t[i];
}
