/* run.config
   OPT: -val -deps -out -input -journal-disable -no-results-function f
   OPT: -val -deps -out -input  -main main3 -journal-disable
   OPT: -val -deps -out -input  -main main4 -absolute-valid-range 0-0xFF -journal-disable
   OPT: -val -deps -out -input  -main main5 -journal-disable
   OPT: -val -deps -out -input  -main main6 -absolute-valid-range 0-0xFF -journal-disable
   OPT: -val -deps -out -input  -main main11  -absolute-valid-range 0-0xFF -journal-disable
   OPT: -val -deps -out -input  -main main8  -absolute-valid-range 0-0xFF -journal-disable

*/
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
  Frama_C_dump_each();
}

int A,B,C,D,E,F,G;
int p[5] = {0,0};
int q[5] = {1,2,3,4,5};

int p2[5] = {0,0};
int q2[5] = {1,2,3,4,5};

int p3[5];

int t,u,v,w,x,y,z,t2,v2,*PTR1,*PTR2,*PTR3,*PTR4,*PTR5, *PTR6;

volatile int c,c1,c2,c3,c4;

void main (void) {
  volatile vol=0;

  /* SECTION 1 */
  A=1;
  B=2;
  f(&A,&B);
  f(&A,&A);
  f(&p,&B);

  /* SECTION 2 */
  x = 1;
  y = 2;
  z = 3;
  PTR1 = c1? &y : &x;
  PTR2 = c2? &y : &z;
  PTR3 = PTR1;

  *PTR1 = 4;
  t = *PTR1;
  *PTR2 = 5;
  v = *PTR1;
  u = *PTR2;
  w = *PTR3;
/* x in {1,4}
   && y in {2,4,5}
   && t = 4
   && v in {4,5}
   && u = 5
   && z in {3,5}
*/

/* SECTION 3 */
  PTR4 = c3? &(p2[1]) : &(q2[2]);
  *PTR4 = 6;
  t2 = *PTR4;
  PTR4 [-1] = 7;
  v2 = *(PTR4+(v2-v2-1));
/* t2 = 6
   && v2 = 7
*/

  p3[1] = vol;
  Frama_C_show_each_d0(p3[1]-vol);
  p3[0] = 0;
  Frama_C_show_each_d2(p3[1]-vol);
}

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
  L: goto L;
  }
}

struct T { struct S s1; struct S s2; struct S s3; struct S s4;} h,i;
void main4()
{
  struct S *p,s,ss,sss;
  struct T *pt,
    t1= {1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11,12},
    t2 ={21,22,23,24,25,26,27,28,29,30,31,32};
  p = c?&(t1.s2):&(t2.s3);
  pt = c?(struct T*)(&(t1.s2)):(struct T*)(&(t2.s3));

  p->a = 777;
  s = *p;

  pt -> s1.b = 888;
  sss = pt-> s1;
  pt = (struct S*)0;
  ss = pt->s1;


  z = 1000;
  u = (c+1)?0:((c+2)?1:2);
  v = u+1;
  x = (c+3)?1:((c+4)?2:5);
  y = x;
  if (y==v)
    {
      z = u - x;
    }
}

void main5()
{
  struct S *p,s,ss,sss;
  struct T *pt,
    t1= {1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11,12},
    t2 ={21,22,23,24,25,26,27,28,29,30,31,32};
    if(c) {
      pt = (struct T*)(&(t1.s2));
      pt -> s1.b = 888;
      }
    else {
      pt = (struct T*)(&(t2.s3));
      pt -> s1.b = 999;
      }

   sss = pt-> s1;


  z = 2;
  u = (c+1)?0:1;
  v = u;
  if (v==w)
    {
      z = u;
    }
}


void main6(void)
{
  int i = 0;
  if (c) PTR1 = &p[1]; else PTR1 = &q[2];
  *PTR1 = 77;
  for (; i<100; i++)
    {
      x = i;
    }
  y = *PTR1;
  PTR2 = (int*) *PTR2;
  if (PTR2 == (char*)PTR1)
    z = *PTR2;
  else
    z = -33;

   u = c?0:1;
   v = u;
   w = v;
   u = (c+1)?0:1;
}


int tz1,tz2,tz3,tx,ty,tz;
void main8(void)
{

  tx = c?2:3;
  ty = tx+1;
  tz = ty+2;

  tz1 = tz==ty+2;
  tz2 = tz==tx+3;
  tz3 = tx==ty-1;

  A = c1 ? 3 : 4;
  B = A + 1;
  y = B == (A+1);
  t = (B + 3) - (A - 1);
  PTR1 = c2 ? &p[2] : &q[3];
  PTR2 = (int*)((unsigned int)PTR1 + 4);
  PTR3 = PTR2 - 1;
  u = *PTR2;
  PTR1[1] = 44;
  v = *PTR2;
  w = *PTR1;
  *PTR1 = 33;
  x = *PTR1;
  z = *PTR3;

  if (c3)
    {
      PTR4 = &q2[1];
      *PTR4 = 33;
      PTR5 = PTR1;
    }
  else
    {
      PTR4 = &q2[2];
      *PTR4 = 44;
      PTR5 = PTR1 + 1;
    }
  C = *PTR4;
  D = *PTR5;
}



union u { long long ll ; int i ; char c ; };

union u U;
char char1;
long long ll1;

void main11(void)
{
  int i = 0;

  PTR3 = &p2[1];
  *PTR3 = 33;
  while (c)
  {
    int * tm = &p2[2];
    *tm = *tm;
    PTR3 = tm-1;
  }
  D = *PTR3;

  f2(p2);

  t = c2?0:1;
  ll1 = (c2+1)?15:16;
  U.ll = ll1 + 1;
  if (c2+2)
    U.i = t + 2;
  else { L: goto L; }

  if (c) PTR1 = &p[1]; else PTR1 = &q[2];
  *PTR1 = 77;
  for (; i<100; i++)
    {
      x = i;
    }
  y = *PTR1;
  PTR2 = (int*) *PTR2;
  if (PTR2 == (char*)PTR1)
    z = *PTR2;
  else
    z = -33;

  PTR4 = &q2[1];
  *PTR4 = 33;
  while (c1++)
  {
    PTR4 = &q2[1];
    *(PTR4-1) = 33;
    }
  A = *(PTR4 - 1);
  B = A - q2[0];
}



