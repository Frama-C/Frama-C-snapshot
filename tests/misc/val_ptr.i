/* run.config
   GCC:
   OPT: -val -deps -out -input -main f -journal-disable
   OPT: -val -deps -out -input -main f1 -journal-disable
   OPT: -val -deps -out -input -main f3 -journal-disable
   OPT: -val -deps -out -input -main f2 -journal-disable
*/
int i,j,x,k,l,m,n,d,a,b,c;
int *p;

void f(int c) {
  j= 16;
  k= 17;
  l= 18;
  a= 11; b = 12; d= 13;


  p = &a;
  if (c) p=&a; else {
    a = 10;
    if (d) p=&b; else p = &d;
    }
  if (a <= 10)
    { j = *p;
    k = a;
      }
  else { k = *p ;};

  i = 10;
}

int T[8],*p;
void f1() {
  for (p=T;p==&T[8];p++)
    *p = 0 ;
}


void f3() {
  p = T;
  if (p + 8 <= &T[8])
    *p = 0 ;
}

void f2(int c)
{
  j = 3;
  a = 1;
  b = 2;
  c = 0;
  if (!c) p = &a; else p = &b;
  if (!p)
    j = *p;

}
