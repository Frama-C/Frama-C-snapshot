/* run.config*
   STDOPT: +"-inout"
*/
int w(int *, int*); int unkn(void);
int A,B,C,D,R,S;

int u()
{
  int ru, wu;
  ru = C;
  return w(&ru, &wu);
}

int v()
{
  int rv, wv;
  rv = D;
  return w(&rv, &wv);
}

int w(int *pr, int *pw)
{
  *pw = A;
  if (unkn()) B = *pr;
  return *pr;
}

int main (int c, int * p) {

  R=u();
  S=v();

  if (c) {
    int x = 1;
    p = &x;
  }
  {
    int y = 0;
    { int z = 1;
      int t = y + z;
    }
  }
  for (int i = 0; i<5; i++) {
    int a = 0;
    a += i;
  }
  return *p;
}
