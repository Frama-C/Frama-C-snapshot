/* run.config
OPT: -wp-mm 1 -wp-debug 1 -journal-disable -wp-no-proof
*/
/* run.config_dev
OPT: -wp-mm 1 -wp-debug 2 -journal-disable -wp-proof
*/

int X, Y;
int T[10];
int * P;

struct Tstr { int a; int * p; } S;

// notice that no guard is generated because there are only direct assigns,
// and direct access in the predicate. So M1 <==> M0.
//@ ensures X + Y + S.a + T[0] == \old(X + Y + S.a + T[0]) + 4;
void vars (void) {
  X ++;
  S.a ++;
  T[0] ++;
  Y ++;
}

// This one cannot be proved because we don't know if P points on X
/*@ ensures *P > X; */
void f_no_hyp (void) {
  int x = X;
  *P = x + 1;
}
/*@ requires \separated (P, &X);
    ensures *P > X;
*/
void f_with_hyp (void) {
  int x = X;
  *P = x + 1;
}
//@ ensures \result == 0; 
int ptr_tab () {
  P = &X;
  T[2] = 0;
  *P = 3;
  return T[2];
}
// Rem : even if the postcondition is about the case where P points on X,
// we cannot prove it with M1 because the guard has to be verified in all cases.
//@ ensures c != 0 ==> \result == 0; 
int ptr_tab_2 (int c) {
  if (c)
    P = &X;
  T[2] = 0;
  *P = 3;
  return T[2];
}
/*@ requires \valid(P);
    ensures *P == \old(*P) + 1 && \result == 3;
*/
int locvar_globptr (void) {
  int x = 3;
  (*P)++;
  return x;
}
/* TODO : there is a problem here with M1 because (*p) in ensure is (*(\old(p))
 * and then p is an alias of old_p --> assign (*p)++ is not ok ! */
/*@ requires \valid(p);
    ensures *p == \old(*p) + 1 && \result == 3;
    */
int locvar_vs_paramptr (int * p) {
  int x = 3;
  (*p)++;
  return x;
}
