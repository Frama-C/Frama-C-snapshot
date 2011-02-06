/* run.config_phoare
 OPT:  -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2 
 */

int T[10];

int read5 (void) {
  return T[5];
}

/*@ requires 0 <= i < 10;
 */
int read (int i) {
  return T[i];
}

int local_array (void) {
  int t [8];
  return t[5];
}

int fext ();

//@ ensures T[0] == \old(T[0]);
int call_fext () { if (fnd()) return 1; else return 0; }

