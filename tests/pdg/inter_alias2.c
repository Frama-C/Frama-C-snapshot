/* run.config
 *    GCC:
 *       OPT: -val -journal-disable -pdg-print -pdg-verbose 2
 *       OPT: -calldeps -fct-pdg incr_ptr -journal-disable -pdg-print -pdg-verbose 2
 *       OPT: -calldeps -fct-pdg f1 -journal-disable -pdg-print -pdg-verbose 2
 *       OPT: -calldeps -fct-pdg f2 -journal-disable -pdg-print -pdg-verbose 2
 */
void incr_ptr (int *p) {
  *p += 1;
}

int f1 (int a) {
  int x1 = a;
  incr_ptr (&x1);
  return x1;
}
int f2 (int b) {
  int x2 = b;
  incr_ptr (&x2);
  return x2;
}
int main (int i1, int i2) {
  int v1 = f1 (i1);
  int v2 = f2 (i2);
  /*@ slice pragma expr v1; */
  return v1 + v2;
}
