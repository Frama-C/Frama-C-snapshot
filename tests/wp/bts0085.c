/* run.config_pruntime
   OPT: -wp -wp-model Runtime -wp-no-logicvar -journal-disable -wp-proof z3 -wp-print -wp-verbose 2
   OPT: -wp -wp-model Runtime -wp-logicvar -journal-disable -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

int * P;
int X;

/*@ requires \valid (P);
    ensures \result == 0;
  */
// how can we say that if \valid (P), P cannot point on local variable x...
int ptr_glob_on_loc (void) {
  int x = 0;
  *P = 3;
  return x;
}

/*@ behavior ok:
      assumes \valid (P);
      ensures \old(*P) == *P;
  @ behavior logicvar:
      // LC: with -wp-logicvar, we have the implicit property that
      // local variables never escape the call-frame ; hence we
      // have separated(&x,p). See ptr_param_vs_glob function below.
      // Hence, the goal should be proved with -wp-logicvar
      ensures \old(*P) == *P;
  */
int ptr_glob_on_loc_2 (void) {
  int x = 0;
  return x;
}

/*@ requires  \valid (p);
  @ ensures \result == 0;
  */
int ptr_param_on_loc (int * p) {
  int x = 0;
  *p = 3;
  return x;
}

/*@ behavior ok:
      assumes \valid (p);
      ensures \old(*p) == *p;
  @ behavior logicvar:
      // LC: with -wp-logicvar, we have the implicit property that
      // local variables never escape the call-frame ; hence we
      // have separated(&x,p). See ptr_param_vs_glob function below.
      // Hence, the goal should be proved with -wp-logicvar
      ensures \old(*p) == *p;
  */
int ptr_param_on_loc_2 (int * p) {
  int x = 0;
  return x;
}

/*@ requires \valid (P);
  @ ensures \result == 0;
  */
int ptr_glob_on_param (int x) {
  x = 0;
  *P = 3;
  return x;
}

/*@ requires \valid (p);
  @ ensures \result == 0;
  */
int ptr_param_on_param (int * p, int x) {
  x = 0;
  *p = 3;
  return x;
}

/*@ ensures \result == 3;
  */
int addr_loc_vs_addr_loc (void) {
  int x, y;
  x = 0;
  y = 3;
  return x + y;
}

/*@ ensures \result == 3;
  */
int addr_loc_vs_addr_param (int x) {
  int y;
  x = 0;
  y = 3;
  return x + y;
}

/*@ ensures \result == 3;
  */
int addr_loc_vs_addr_glob (void) {
  int y;
  X = 0;
  y = 3;
  return X + y;
}

/*@ ensures \result == 3;
  */
int addr_param_vs_addr_glob (int x) {
  x = 0;
  X = 3;
  return x + X;
}

//@ ensures ! \valid(P);
void invalid_local_addr (void) { 
  int x; 
  P = &x; 
}

//@ ensures ! \valid(P);
void invalid_param_addr (int x) { 
  P = &x; 
}

/*@ requires \valid (P); */
void disj_glob_addr_param (int x) {
  //@ assert (P != &x);
}

/*@ ensures ok: \separated (p, &X) ==> X == \old(X);
    ensures ko: X == \old(X);
*/
int ptr_param_vs_glob (int * p) {
  *p = 0;
  return *p;
}

int main (void) {return 0;}
