
/*@ requires \valid(p);
  @ ensures *p == \old(i);
  @ */
void gi(int *p, int i) {
  *p = i;
}

/*@ behavior zero:
  @   assumes i == 0;
  @   ensures \result == 0;
  @ */
int fi(int i) {
  gi(&i, i);
  return i;
}

/*@ requires \valid(p) && \valid(*p) && \valid(i);
  @ ensures **p == \old(*i) && *p == \old(i);
  @ */
void gp(int **p, int *i) {
  **p = *i;
  *p = i;
}

/*@ behavior zero:
  @   assumes i == 0 && \valid(p);
  @   ensures \result == 0;
  @ */
int fp(int i, int* p) {
  gp(&p, p);
  /*@ assert i == 0; */
  *p = 0;
  return i;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make addrof_param"
End:
*/
