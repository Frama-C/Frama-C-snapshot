/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32
*/

int main(int x) {
  int *p,*q;
  int tab[10];

  *p = 3;
  q = p;
  *q = *p + 5;

  tab[3] = *q;
  tab[x] = *q;

  p = &tab[2];
  p = &tab[x];

  *(p+1) = tab[0];
  *(p+1) = tab[x];

  *q=p[2];

  return 0;
}
