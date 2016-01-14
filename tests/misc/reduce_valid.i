int t[2], u[2];
int *p,*q,*r, A, offs;

void main(int c, int d, int e, int f, int g, unsigned short h)
{


  p = c ? t : (void*)0;
  *p = 2;
  p[1] = 3;
  *p = 4;
  
  q = (void*)0;
  if (d)
    {
      Frama_C_show_each_ici(0);
      *q = 3;
      *q = 4;
      Frama_C_show_each_la(0);
    }
  
  r = e ? (f ? t : t+1) : (void*)0;
  offs = g ? 1 : 2;
  A = r[offs];
  Frama_C_show_each_r(r);
  u[h+1] = 1;
}
