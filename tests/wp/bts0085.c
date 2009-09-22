/* run.config
OPT: -wp-mm 2 -wp-debug 1 -journal-disable -wp-no-proof
*/
/* run.config_dev
OPT: -wp-mm 2 -wp-debug 2 -journal-disable -wp-proof
*/

int * P;
int X;

/*@ requires \valid (P);
  @ ensures \result == 0;
  */
// how can we say that if \valid (P), P cannot point on local variable x...
int ptr_glob_on_loc (void) {
  int x = 0;
  *P = 3;
  return x;
}

/*@ requires \valid (p);
  @ ensures \result == 0;
  */
int ptr_param_on_loc (int * p) {
  int x = 0;
  *p = 3;
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
