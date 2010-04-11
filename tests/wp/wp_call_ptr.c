/*  assigns *p; */
int * P;
/*@
     ensures *P == 0;
*/
void razP (void) {
  *P = 0;
}
/*@
     ensures *p == 0;
*/
void raz (int * p) {
  *p = 0;
}
/*@  assigns *p;
     ensures *p == \old(*p) + 1;
*/
void incr (int * p) {
  (*p)++;
}

//@ ensures \result == 0;
int call_raz (void) {
  int x;
  raz (&x);
  return x;
}
int main (void) { return 0 ; }
