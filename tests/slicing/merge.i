/* run.config
   EXECNOW: make -s tests/slicing/merge.cmxs
   CMD: @frama-c@ -load-module tests/slicing/libSelect.cmxs -load-module tests/slicing/libAnim.cmxs -load-module tests/slicing/merge.cmxs
   OPT: -deps -slicing-level 3 -journal-disable
*/

int G1, G2, G3;

void init (int a1, int a2, int a3);
void add (int a1, int a2, int a3);
void g (int a1, int a2, int a3);

void init (int a1, int a2, int a3) {
  G1 = a1; G2 = a2; G3 = a3;
}
void add (int a1, int a2, int a3) {
  G1 += a1; G2 += a2; G3 += a3;
}

void g (int a1, int a2, int a3) {
  init (a1, a2, a3);
  add (a1, a2, a3);
}

void main (int x, int y, int z) {
  g (x, y, z);
}
