/* run.config
   OPT: -rte -rte-print -machdep x86_32 -journal-disable
*/

int main() {
  int *p,*q;
  int tab[10];
  int x = 0;

  *p = 3;
  q = p;
  *q = *p + 5;

  tab[3] = *q;

  p = &tab[2];

  *(p+1) = tab[0];

  *q=p[2];

  return 0;
}
