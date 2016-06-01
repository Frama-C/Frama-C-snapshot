int A,B,C,D,E;

int f(int x)
{
  B = A;
  C = x;
  return C;
}

int main(void)
{
  A = D;
  f(E);
  return 0;
}
