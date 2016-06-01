/* run.config
   OPT: -rte -then -print
 */

extern void f(int* p);
int i;
unsigned int j;

int main(void) {
  int *p;
  int tab[10];

  *p = 3;
  tab[i] = *p;
  *(p + 1) = tab[j];

  return 0;
}
