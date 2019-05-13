/* run.config*
   STDOPT: +"-eva-warn-copy-indeterminate=-main -eva-warn-key garbled-mix -absolute-valid-range 0x2D-0x30 -eva-no-warn-pointer-subtraction" +"-then -absolute-valid-range 0x2D-0x31"
*/



int t[10],x,y,z,zz;
int p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17;
unsigned int u1,u3;
int * q1;

int quo1,rem1,quo2,rem2,quo3,rem3,quo4,rem4,quo5,rem5,mm1,mm2,mm3,quo6,c1,c2,qu1,qu2;
long long ll1,ll2;
struct {int a; int b:2; } tt[5];
int ttt[5][6];

int square;

int main(int u2, int u3, int u4)
{
  z = 37;

  quo1 = z/12;
  rem1 = z%12;
  quo2 = (-z)/12;
  rem2 = (-z)%12;
  quo3 = (-z)/(-12);
  rem3 = (-z)%(-12);
  quo4 = (z)/(-12);
  rem4 = (z)%(-12);
  quo5 = (z-1)/(-12);
  rem5 = (z-1)%(-12);

  p1 = (int)(&p2 - &p3);

  p2 = ~((int)&p1);

  p3 = (int) &(t[(char)(&p1)]);

  p4 = (int) &(tt[(char)(&p1)].a);

  p5 = (int) &(ttt[(char)(&p1)][(char)&p2]);

  p6 = (int) &(ttt[(char)(&p1)][u2]);

  p7 = (int) &(ttt[u2][(char)(&p2)]);

  p8 = (&p1 + 1) < &p2;

  p9 = (int)&p1 / 2 ;

  p10 = 12 & ((int)&p1);

  if (u2 < 0) p11 = u2 & (-4);

  p12 = (int)&p1 & (int)(&p2);

  q1 = &p1;
  p13 = *((char*)&q1)+2;

  p14 = *((char*)&q1)+2;

  tt[0].b = 3;
  p15 = tt[0].b;

  t[1] = **((int**)(45));
  p16=2+*((int*)((char*)t+2));

  {
    int s,t ;
    if ((u3 <= 15) && (u3 >= -10))
      s = u3;
    else s = 0;
   if ((u2 <= 100) && (u2 >= -150))
      t = u2;
    else t = 0;
    mm1 = (16+32*t) * (2+3*s);
    mm2 = (4+32*t) * (16+96*s);
    mm3 = (1+15*t) * (1+35*s);
    quo6 = (2007+15*s) / (-5);
    qu1 = (2007+15*s) / (20 + s);
    qu2 = (7+15*s) / (20 + s);
    ll1 = (long long)(5*s+3) + 0xFFFFFFFFL;
    ll2 = (long long)(5*s+1) + 0x100000003L;
    c1 = (int)ll1;
    c2 = (int)ll2;
    Frama_C_show_each_1(s);
    //@ assert  (s >= 0) || (s < 0) ;
    square = s * s;
  }

  u2 = 34;
  u1 = u2 >> 2 ;

  2[t]=3;

  p17 = -0x80000000;
  if (u4 & 1) p17 %= -1;

  return (*(2+t)) + t[2];
}
