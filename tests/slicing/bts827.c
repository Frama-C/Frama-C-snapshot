/* run.config
   OPT: -check -slice-return main -journal-disable -then-on 'Slicing export' -print
*/

/* The problem was a mix-up between f outputs and retrun value. */

int G;

int f (void) {
  G = 3;
  return 5;
}

int main (void) {
  G = 1;
  G += f ();
  return G;
}

