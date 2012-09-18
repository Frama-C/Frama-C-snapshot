/* run.config
   OPT: -load-script tests/pdg/dyn_dpds.ml -deps -journal-disable -pdg-print -pdg-verbose 2
*/


/*
   To have a look at the dot PDG :
   bin/toplevel.byte -deps -pdg-dot pdg -fct-pdg main tests/pdg/dyn_dpds.c ;
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
