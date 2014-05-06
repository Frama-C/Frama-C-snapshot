int f(int b);
int g(int *a)
{
  int x ;
  x = sizeof(f(a));
  switch (x) {
    case (sizeof(x++)): return 1;
    default: return 0;
  }
  return x;
}
