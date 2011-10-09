/* run.config
   EXECNOW: make -s tests/slicing/mark_all_slices.opt
   CMD: tests/slicing/mark_all_slices.opt
   OPT: -check -deps -slicing-level 3 -no-slice-callers -journal-disable
*/
int A, B, C, D;
int A2, B2, C2, D2;

int all (int x) {
  A = x;
  B = x;
  C = x;
  D = x;
  return x+1;
}

int fA (void) { all (1); return A; }
int fB (void) { all (1); return B; }
int fC (void) { all (1); return C; }
int fD (void) { all (1); return D; }

int main (void) {
  A2 = fA();
  B2 = fB();
  C2 = fC();
  D2 = fD();
  return all (0);
}
