/* run.config
   OPT: -rte -warn-signed-overflow -print -rte-precond
*/

int main() {
  int x, y,z ;
  int *p;
  int tab[10];

  x = sizeof(*p);

  y = sizeof((double) *p);

  z = sizeof(tab[3]);

  return 0;
}
