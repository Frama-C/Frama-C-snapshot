/* run.config
   STDOPT: +"-remove-redundant-alarms"
   OPT: -rte-all -rte -then -val -remove-redundant-alarms
*/
int X,Y,Z1,Z2,T,U1,U2,V,W1,W2;
int a,b,d1,d2,d0,e;
int t[5]={1,2,3};
int *p;

void main (void)
{
  int i;
  volatile int c=0;
  while (c+1)
    {
      if (c) X++;
      if (c+2) X--;
    }
  Y = -5;
  if ((X>=Y)  && (X<=12) )
    Y = X;
  Y = 27 * Y + 9;
  Z1 = Y / 3;
  Z2 = Y / 5;
  V = Y + 1;
  W1 = V / 3;
  W2 = V / 5;
  T = Y + 160;
  U1 = T / 3;
  U2 = T / 5;
  p = &(t[3]);
  a = 40000/Z2;
  b = ((int)&Z2)/Z2;
  d2 = 100 / (int)(&X + 2);
  d1 = 100 / (int)(&X + 1);
  d0 = 100 / (int)(&X);
  e = - (int) &X;
}
