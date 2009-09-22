int a[12][45] = {0, };

//@ assigns a[..][..];
void f(void)
{
  a[1][3] = 42;
}

void main(void)
{
  f();
}
