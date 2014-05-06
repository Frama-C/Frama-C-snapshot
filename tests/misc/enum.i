/* run.config
  GCC:
  OPT: -val -deps -out -input  -main f -journal-disable
*/
typedef enum counter {ZERO,ONE,TWO,LAST=TWO};

int t [LAST + 1] = { 1 };
int u [TWO + 1] = { 2 };

void f(void)
{
  int i[3]={0};
  t[2] = 42;
  u[TWO] = 36;
  enum counter j=0;
  for(j=0;j<2;j++)
    i[j] = 1;

  enum counter k = ZERO;
  //@ assert k == ZERO;
}
