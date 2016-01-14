/* run.config_qualif
   DONTRUN: (nothing to prove)
*/

int X,Y;

void g (void) {
  X ++;
}

void f (int x, int y) {
  if (x > X) {
    g ();
    //@ assert KO: ax2: x > X ;
  }
}
