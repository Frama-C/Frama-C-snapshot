/* run.config
   EXECNOW: make -s tests/pdg/dyn_dpds.opt
   CMD: tests/pdg/dyn_dpds.opt
   OPT: -deps
*/

/*
   To have a look at the dot PDG :
   bin/toplevel.byte -deps -pdg-debug "-dot-pdg pdg -fct-pdg main" tests/pdg/dyn_dpds.c ;
   zgrviewer pdg.main.dot

   or use tests/pdg/dyn_dpds.ml to test the dynamic dependencies.
*/

int G;

int main (int a, int b, int c) {
  int x;
  int * p ;
  x = a + b;
  p = &x;
  if (c < 0) {
    x = -x;
    //@assert (*p > G);
  }
  return x;
}
