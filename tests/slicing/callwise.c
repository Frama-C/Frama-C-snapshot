/* run.config
   OPT: -check -calldeps -slice-return main -slicing-level 2 -journal-disable -then-on 'Slicing export' -print
*/

int a = 1, b = 1, c = 1, d = 1, *p;

void f(int *p, int *q)
{
  *p  += *q;
}

int choose (int cond, int x, int y) {
  return cond ? x : y;
}

void fs163_f (int *p,int n) {
  *p = n;
}

int fs163_main (int n) {
  int A,B,C;
  int T[5];

  fs163_f (&A, 1);
  fs163_f (&B, n);
  fs163_f (&C, 2);
  fs163_f (&T[0], 0);
  fs163_f (&T[1], 1);
  for (int i=0; i<5; i++)
    fs163_f (&T[i],i);

  return T[3];
}

int main(void)
{
  int n = 2, m = 3;

  f(&a, &b);
  f(&c, &d);

  b = choose (0, n, m);
  a += choose (1, n, m);

  a += fs163_main (10);

  return a;
}
