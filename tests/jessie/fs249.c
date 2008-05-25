int x,*p,t[10];

void CEA_F(int);

void main(void)
{
  volatile int y=0;
  t[1]=y;
  CEA_F(t[1]-y);
  t[0]=0;
  CEA_F(t[1]-y);
}

/* 
Local Variables:
compile-command: "LC_ALL=C make fs249"
End:
*/
