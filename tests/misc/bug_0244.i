int R,*p;

void main(void)
{
  int a,i;
  a=2;
  for(i=0; i<2; i++)
  {
    int u=a;
    p = &u;
  }
  R = *p;
}
