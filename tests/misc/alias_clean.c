int x,*p,t[10];
void main(void)
{
  volatile int y=0;
  t[1]=y;
  CEA_F(t[1]-y);
  t[0]=0;
  CEA_F(t[1]-y);
}
