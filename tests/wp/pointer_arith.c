int T[10];

int x;

/*@ behavior no_alias :
      assumes i != j;
      ensures \result == \old(T[j]);  
    behavior with_alias :
      assumes i == j;
      ensures \result == 1;
*/
int tab_read_write (int i, int j) {
  T[i] = 1;
  return T[j];
}

/*@ requires \valid (p) && \valid (q) && p != q;
    ensures \result == \old(*q) && *p == 3; */
int no_alias (int * p, int * q) {
  *p = 3;
  return *q;
}

/*@
  requires \valid (p) ;
  ensures \result == 3 ;
*/
int wptr (int *p) {

  *p = 3;
//  x = *p;
  return *p;

}

void cmp_addr_loc () {
  int i, j;
  //@ assert &i != &j;
  if (&i < &j) return;
  //@ assert &i >= &j;
  if (&i - &j > 0) return;
  //@ assert \false;
}

//@ ensures i == j ==> \result == 0;
int cmp_ptr (int i, int j) {
  int * pi = T+i;
  int * pj = T+j;
  if (pi < pj) return -1;
  if (pi - pj > 0) return 1;
  return 0;
}
