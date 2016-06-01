int *p,*q;
int a,b;
int r=2, s; volatile int v;

int f(int x, int y, int z)
{
  return x;
}

int g(int x, int y, int z)
{
  return y;
}

int (*t[2])(int, int, int) = { f, g};

int main (int c, int d, int i0, int i1, int i2, int i3) {
  p=c?&a:(int*)3;
  q=d?&b:(int*)2;
  if (v) r = *((p+ (int)q));

  s = (t[i0])(i1, i2, i3);

  return ((int)(p+ (int)q));
}
