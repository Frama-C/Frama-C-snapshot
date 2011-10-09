int *X, *Y, *Z, *T, *U, *V;

int * f(void)
{
  int a,b,c;
  X = &a;
  return &b;
}

int *g(void)
{
  volatile int d=0;
  T = f();
  U = d ? T : &d;
  return U;
}

int *h(int *x)
{
  return x+1;
}

void main(void)
{
  int e;
  Y = f();
  Z = g();
  Frama_C_dump_each();
  V = h(&e);
}
