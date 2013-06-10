/* run.config
   STDOPT: +"-slevel 4"
*/
int R,*p,S,*q;

void main(int c, int d)
{
  int a,i;
  a=2;
  p = q = &a;
  for(i=0; i<2; i++)
  {
    int u=a;
    p = &u;
    toto:
    {
      int v;
      v = 3;
      v++;
      q = &v;
    }
  }
  if (c) R = *p;
  if (d) S = *q;
  //if (a-a) goto toto;
}
