/* run.config
   GCC:
   OPT: -inout -deps -main appel_ptr_fct -fct-pdg appel_ptr_fct -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -inout -deps -main appel_ptr_fct_bis -fct-pdg appel_ptr_fct_bis -journal-disable  -pdg-print -pdg-verbose 2
*/

extern int G1, G2, G3, G4;

int fct1 (int x, int y, int z) {
  G1 = z;
  G3 = y;
  G4 = z;
  return x;
}
int fct2 (int x, int y, int z) {
  G2 = z;
  G3 = x;
  return y;
}
int appel_ptr_fct (int c, int d) {
  int a = 1, b = 2;
  int (*pf) (int, int, int) = c ? &fct1 : &fct2;
  int x = (*pf)(a, b, d);
  return x + G1 + G2;
}
int appel_ptr_fct_bis (int c, int a, int b, int d) {
  int (*pf) (int, int, int) = c ? &fct1 : &fct2;
  G4 = (*pf)(a, b, d);
  return G4 ;
}
