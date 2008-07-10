/* run.config
   OPT: -slice-return main -slice-print -calldeps
   DONTRUN: something to do to have better results...
*/
int T[10];

int f (int i) {
  T[i] ++;
  return T[i];
}

int main (void) {
  int x1 = f(1);
  int x2 = f(2);
  return x2;
}
