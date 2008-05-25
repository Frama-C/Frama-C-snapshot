int t[2];
int *p,*q;

void main(int c, int d)
{
  p = c ? t : (void*)0;
  *p = 2;
  p[1] = 3;
  *p = 4;
  
  q = (void*)0;
  if (d)
    {
      CEA_ici(0);
      *q = 3;
      *q = 4;
      CEA_la(0);
    }
}
