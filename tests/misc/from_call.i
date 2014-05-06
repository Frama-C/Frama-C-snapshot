/* run.config
   OPT: -calldeps -users -val -journal-disable -input
   OPT: -deps -show-indirect-deps -journal-disable
*/
int a,b,c,d;
int x,y,z,t;

int g(int w)
{
  return w + t;
}

int h(int);

int f(int *p)
{
  static int * previous = &a;
  *p = *previous;
  previous = p;
  return g(h(x)+*p);
}

int A1,A2,A3,A4,A5,A6,A7,A8;
int R1,R2,R3,S1,S2,S3;
int T0,T1,T2;

int dispatcher(int c, int y, int z, int x)
{
  return c ? y : z;
}

int return_A1(void)
{
  return A1;
}

int return_A2(void)
{
  return A2;
}

int dispatcher2(int c)
{
  return c ? return_A1() : return_A2();
}

int call_dispatcher2_1(void)
{
  return dispatcher2(1);
}

int call_dispatcher2_0(void)
{
  return dispatcher2(0);
}

int call_dispatcher2(int r)
{
  return dispatcher2(r);
}

int tab[5];

int access_tab(int ind)
{
  return tab[ind];
}

int AA,AR,AS;
int At[2]={&AA};
int Ar[2]={&AA};
int *Ap=At;

/*@ assigns AR \from Ap[..] ;
    assigns AS \from Ar[..] ;
 */
void unavailable_f(void);

void main(int r)
{
  y = f(&b);
  z = f(&c) + f(&d);
  R1 = dispatcher(1,A1,A2,A3);
  R2 = dispatcher(0,A3,A4,A6);
  R3 = dispatcher(r,A4,A5,A7);
  S1 = call_dispatcher2_1();
  S2 = call_dispatcher2_0();
  S3 = call_dispatcher2(r);
  tab[0]=A1;
  tab[1]=A2+A3;
  tab[2]=A4;
  T0 = access_tab(0);
  T1 = access_tab(1);
  T2 = access_tab(2);

  unavailable_f();
}
