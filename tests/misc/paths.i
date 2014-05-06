/* run.config
   OPT: -experimental-path-deps -deps -journal-disable
*/
int a,b,c,d,e,i,d1,d2,d3,d4,X1,X2,X3,X4,X5,X;

void f1(void)
{
  X = X1;
  if (d1) X = X4;
}

void f2(void)
{
  X = X2;
}

void f3(void)
{
  X = X3;
}

int f(int fx, int fy, int fz)
{
  d2 = fx;
  if (fy) i++;
  return d3;
}

void (*t[3])(void)={f1, f2, f3};

/*@ assigns \result \from x ; */
int unknownfun(int x);

int main(int r,int s,int u,int v,int w,int x,int y,int z,int ww){
  d1 = x;
  c = u?a:b;
  d = b + v;
  d4 = unknownfun(ww);
  if (d4)
    i++;
  r++;
  if (d)
    a=1;
  (t[w])();
  if (X)
    i++;
  d3 = z;
  if (f(y,s,r))
    i++;
  if (d2)
    i++;
  return 0;
}
