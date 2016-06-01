
int i,j,k;

void f(void)
{
  for (i=0;i<1000;i++);
}

void main(void)
{
  for (j=0;j<1000;j++)
    f();
}
