/* run.config
   GCC:
   OPT: -lib-entry -main g -pdg -dot-pdg tests/pdg/call  -journal-disable
*/

/* Ne pas modifier : exemple utilisé dans le rapport. */

/*BDOC*/
struct {int a; int b; } G;
int A, B;

int f (int a, int b) {
    G.b = b;
    return a + G.a;
}

int g (int x, int y, int z) {
  int r =  f (x+y, z);
  A = G.a;
  B = G.b;
  return r;
}
