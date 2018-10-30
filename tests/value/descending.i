int f(int A[], int n)
{
  int i = 0;
  while (i < n)
    A[i++] = 0;
  return i;
}

int test1()
{
  int A[10];
  int i = f(A, 10);
  return A[i-1];
}

void test2()
{
  int A[33];
  int i;
  int n = 31;

  for (i = 0 ; i < n ; i++)
  {
    A[i]  = 0;
  }

  int j = i + 1;
  A[j] = 1;
}

void main(void)
{
  test1();
  test2();
}
