/* run.config
   GCC:
   OPT: -val -deps -out -input -main f -journal-disable
   OPT: -val -deps -out -input -main f1 -journal-disable
   OPT: -val -deps -out -input -main f2 -journal-disable
*/
int i,j,x,k,l,m,n,d;
void f(int c){
  int j = 12;
  if (c) x=1; else x = -1;

  if (x<=-2) j = x;

  i = 10;
}

void f1(int c){
  j= 13;
  k= 14;
  l= 15;
  if (c) x=1; else x = -1;

  if (x<=0)
    {j = x;
    if (x<=-2) k = x;
    l=x;
      }

  i = 10;
}

void f2(int c) {
  j= 16;
  k= 17;
  l= 18;
  if (c) x=1; else {
    if (d) x=2; else x = 3;
    }

  if (x <= 1 || x>=3  )
    { x = 2;
    j = x;
      }
  else { x++ ; k = x;};

  i = 10;
}
