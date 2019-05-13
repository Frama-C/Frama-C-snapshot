/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   STDOPT: +"-load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -deps"
*/


/*
   To have a look at the dot PDG :
   bin/toplevel.byte -deps -pdg-dot pdg -fct-pdg main @PTEST_DIR@/@PTEST_NAME@.c ;
   zgrviewer pdg.main.dot

   or use @PTEST_DIR@/@PTEST_NAME@.ml to test the dynamic dependencies.
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
