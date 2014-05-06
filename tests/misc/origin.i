/* run.config
   GCC:
   OPT: -val -out -deps -main main -journal-disable
   OPT: -val -out -deps -main origin -journal-disable

*/
char f();

int a, b, aa2, *p, *pa1, *pa2, *qa2, *pa3, *q;

int t[12], tt[10], ta1[10], ta2[10], ta3[10], tta2[10];

void origin_arithmetic_1(void) {
  pa1 = (int*)(-(int)ta1);
  *pa1 = 0;
}
/************/
void origin_arithmetic_2(int c1) {
  pa2 = (int*)(-(int)ta2);
  qa2 = c1 ? pa2 : (int*)(-(int)tta2);
  *qa2 = &aa2;
}
/************/
void origin_arithmetic_3(void) {
  pa3 = (int*)(-(int)ta3);
  *pa3 = 3;
}


int g(void);
int *gp(void);

int l1, l2, l3, *pl;

void origin_leaf_1 () {
  l1 = g();
}

int * Tm1[2] ={&a, &b};
int * Tm2[2] ={&a, &b};
int * Tm3[2] ={&a, &b};
int * Tm4[2] ={&a, &b};
int *pm1, *pm2, *qm2;

void origin_misalign_1(void) {
  pm1  = *(int**)(2 + (char *) Tm1);
  *pm1 = 1;
}

void origin_misalign_2(void) {
  pm2  = *(int**)(2 + (char *) Tm2);
  qm2 = pm2+1;
  *qm2 = (int)&a;
}

int *pun, *pun2, *qun2;

void origin_uninitialized_1(int c1) {
  int i, * pi ;
  if (c1)
    pi = &a ;
  pun = pi;
}

void origin_uninitialized_2(int c1, int c2) {
  int i, * pi ;
  if (c1)
    pi = &a ;
  pun2 = pi;

  if (c2)
    qun2 = pun2 + i;
}

volatile int random;
int esc1, esc2, esc3, esc4, esc5;
void local_escape_1(int arg)
{
  int local1, local2;
  esc1 = (int) &arg;
  esc2 = (int) &local1;
  esc3 = - (int) &arg;
  esc4 = random ? esc2 : 12;
  local2 = (int) &local1;
  esc5 = (int) &esc1;
}


void main(int c1, int c2)
{
  origin_arithmetic_1();
  origin_arithmetic_2(c1);
  origin_arithmetic_3();
  origin_leaf_1 ();
  l2 = l1;
  l2 += g();
  pl = gp();
  l3 = *pl;
  origin_misalign_1();
  origin_misalign_2();
  p =          *(int**)(2 + (char *) Tm3);
  q = c1 ? p : *(int**)(3 + (char *) Tm4);
  origin_uninitialized_1(c1);
  origin_uninitialized_2(c1, c2);
  local_escape_1(12);
}


/************************************/
int x, y;
struct st {
  char  c;
  short i;
  int  *p, *t[2];
} v = { 1, 2, &x, &y};

struct st origin (int c0) {
  struct st r;
  int *q1, *q2;

  r.c = f() ;
  r.i = c0 ;
  r.p = *(int *) (&v.c + 3);
  q1 =           *(int**)(2 + (char *) v.t);
  q2 = c0 ? q1 : *(int**)(3 + (char *) v.t);
  r.t[0] = q2 ;
  r.t[1] = (int *)(- (int)&x) ;
  return r;
}
/************************************/
