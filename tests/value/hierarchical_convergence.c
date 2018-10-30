/* run.config*
   STDOPT: +" -eva-hierarchical-convergence"
*/

int f(int n) {
  int i, j;
  for (i = 0 ; i < n ; i++) {
    Frama_C_show_each(i);
    for (j = 0 ; j < i ; j++) {
      Frama_C_show_each(i,j);
      // Nothing
    }
  }

  return i * j;
}

void main(void) {
  f(100);
}
