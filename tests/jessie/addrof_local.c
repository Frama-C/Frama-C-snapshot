
/*@ requires \valid(p);
  @ ensures *p == \old(i);
  @ */
void gi(int *p, int i) {
  *p = i;
}

/*@ ensures \result == 0;
  @ */
int fi() {
  int i = 0;
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

/*@ ensures \result == 0;
  @ */
int fp() {
  int i = 0;
  int *p = &i;
  gp(&p, p);
  /*@ assert i == 0; */
  *p = 0;
  return i;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make addrof_local"
End:
*/
