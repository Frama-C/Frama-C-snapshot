/* run.config
*    OPT: -check -slice-return main -journal-disable -then-on 'Slicing export' -print
*/

int f0 (void) {
  int i = 0;
  int x; 
  if (i) { x = 1; L: x++; }
  else { x = 0; goto L; }
  return x;
}
int f1 (void) {
  int i = 1;
  int x; 
  if (i) { x = 1; goto L; }
  else { x = 0; L: x++; }
  return x;
}

int main (int n) {
  return f0 () + f1 ();
}
