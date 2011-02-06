/* run.config
   OPT: -rte -rte-print -rte-precond -journal-disable
*/

int main() {
  int x, y,z ;
  int *p;
  int tab[0];

  x = sizeof(*p);

  y = sizeof((double) *p);

  z = sizeof(tab[3]);

  return 0;
}
