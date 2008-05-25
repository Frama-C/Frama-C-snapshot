void f(char*x,int*y) {
  (*x)++;
  *x++;
  (*x)++;
  (*y)++;
}

void f2(char*x) {
  char *q;
  (*x)++;
  q = x+1;
  (*q)++;
  //  Frama_C_dump_each();
}

int A,B,C,D,E,F,G;
int p[5] = {0,0};
int q[5] = {1,2,3,4,5};

int p2[5] = {0,0};
int q2[5] = {1,2,3,4,5};

int t,u,v,w,x,y,z,t2,v2,*PTR1,*PTR2,*PTR3,*PTR4;

volatile int c,c1,c2,c3,c4;

struct S { int a; int b; int c; } e,g;

void main3()
{
  struct S *p,*q,s1={2,4,6},s2={1,3,5};
  p = c?&s1:&s2;
  p-> a = 7;
  t = p->b;

  z = 2;
  u = (c+1)?0:1;
  v = u;
  if (w==v)
    {
      z = u;
    }

 
  PTR1 = & ( p2 [(c+1)?0:((c+2)?1:2)] );
  PTR2 = PTR1+1;
  *PTR1 = (c+10) ? 96 : (c+11) ? 97 : 98;
  PTR3 = p2 + ((c+3)?1:((c+4)?2:4));
  *PTR3 = 99;
  PTR4 = PTR3;
  x = *PTR1;
  if (PTR4==PTR2)
    {
      t2 = *PTR1;
      v2 = PTR3 - PTR1;
     }
  else{
  }
}


int tz1,tz2,tz3,tx,ty,tz;

/* 
Local Variables:
compile-command: "LC_ALL=C make fs248_short"
End:
*/
