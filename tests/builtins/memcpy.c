/* run.config*
   STDOPT: +"-calldeps -slevel-function init:2000 -eva-msg-key imprecision -plevel 150 -main main_all -inout -no-deps -absolute-valid-range 100000-100001 -then -load-module report -report"
*/
#include "string.h"

volatile int i;
char src[20];
char dst1[20], dst2[20], dst3[20];
char dst4[20], dst5[100];

void init () {
  int j;
  for (j=0;j<20;j++) {
    src[j] = j+1;
    dst1[j] = -1;
    dst2[j] = -1;
    dst3[j] = -1;
    dst4[j] = -1;
  }
  for (j=0;j<100;j++) dst5[j] = -1;
}

volatile maybe;

void buggy () {
  char c;
  char *p = maybe ? &c: "abc";
  memcpy(p,"d",1);
}

int tm[1000];
int um[1000];

typedef struct {
  short ts;
  int ti;
} typ;

typ ttyp[1000];

void many() {
  char s[] = "abcd";
  unsigned int p = maybe;
  //@ assert p < 1000;

  tm[0]=0;
  memcpy(&tm[p],s,4);
  um[0]=0;
  memcpy(&um[p],s,2);

  typ ty = {1, 2};
  ttyp[0] = ty;
  memcpy(&ttyp[p],&ty,sizeof(typ));
}

struct t1 { int x; int y; int* p; char padding[24];} v1,v2, v3, v4, v5;
struct t1 t[4];


void main (int a, int b){
  buggy ();

  many ();

  init ();

  //@ assert 5 <= b && b <= 15;
  memcpy(dst1+1, src+2, b);

  memcpy(dst2+1, src+2, 2*b);

  //@ assert 5 <= b && b <= 14;
  memcpy(dst3+5, src+2, b);

  memcpy(dst4+5, src+2, 2*b);

  v2 = v2;
  v2.p = &v1.y;
  t[1]=v2;

  v1.x = 5;
  v1.y = 7;
  memcpy(&v2, &v1, sizeof(v1));

  memcpy(t+2, t, (1+!a)*sizeof(v1));

  memcpy(&v3, t+(int)t, sizeof(v1));

  memcpy(&v4 + (int)&v4, &v1, sizeof(v1)-20);
  v4.y = (int) &t[0];
  memcpy(&v5 + (int)&v5, &v4, sizeof(v4)-20);

  if (maybe) {
    int x=1;
    while(1)
      memcpy((void *)&x, (void const*)&x, i);
  }  

  char *p;
  p = maybe ? &dst5[0] : &dst5[20];
  memcpy(p, &src[0], b);
  b = maybe;
  //@ assert 1 <= b < 20;
  p = maybe ? &dst5[40] : &dst5[70];
  memcpy(p, &src[0], b);

  // Destination pointer is unbounded
  char ptop1[800];
  int *pptop = ptop1;
  while (1) {
    pptop++;
    if (maybe) break;
  }
  memcpy(pptop, src, 4);

  char ptop2[800];
  pptop = &ptop2[750];
  while (1) {
    pptop--;
    if (maybe) break;
  }
  memcpy(pptop, src+1, 4);

  char ptop3[800];
  pptop = &ptop3[2];
  while (1) {
    if (maybe) pptop--;
    if (maybe) pptop++;
    if (maybe) break;
  }
  memcpy(pptop, src+2, 4);

  char ptop4[800];
  pptop = &ptop4[2];
  while (1) {
    if (maybe) pptop--;
    if (maybe) pptop++;
    if (maybe) break;
  }
  memcpy(pptop, src+2, 5);

  // Size is a garbled mix
  char garbledsize[100];
  int* pgarbledsize = &garbledsize[10];
  memcpy(pgarbledsize, src, (unsigned int)garbledsize);

  // Sure size may be zero
  char dstmaybesize1[15], dstmaybesize2[150];
  int maybesize = maybe;
  //@ assert 0 <= maybesize <= 22; // >= plevel / 10
  memcpy(dstmaybesize1, src, maybesize);
  //@ assert 0 <= maybesize <= 6;
  memcpy(dstmaybesize2, src, maybesize);
}


/*@ assigns \result \from l, u;
  ensures l <= \result <= u; */
int itv(int l, int u);

/*@ requires \valid(p + (0 .. l-1));
   assigns p[0 .. l-1] \from maybe;
   ensures \initialized(p + (0 .. l-1));
*/
void make_unknown(unsigned char *p, size_t l);

void main_uninit () {
 unsigned char a[50];
 unsigned char b[50];
 int r = 0;
 if (maybe) {
   memcpy(b, a, 10);
   //@ assert !\initialized(&b[8]);
   memcpy(b, a, itv(0,25));
   //@ assert !\initialized(&b[11]);
 }
 else if (maybe) {
   make_unknown(a, 10);
   memcpy(b, a, 10);
   //@ assert \initialized(&b[8]);
   memcpy(b, a, itv(0,25));
   r += b[11]; // initialisation unknown
 }
 else if (maybe) {
   make_unknown(b, 10);
   if (maybe) {
     memcpy(b, a, 10); // de-initialize b
     //@ assert !\initialized(&b[8]);
   } else {
     memcpy(b, a, itv(0,25)); // copy completely uninitialized in an unsure way
     //@ assert !\initialized(&b[11]); // already NOT initialized
     r += b[8]; // initialisation unknown
   }
 }
 else if (maybe) {
   make_unknown(a, 10);
   make_unknown(b, 10);
   memcpy(b, a, 10);
   //@ assert \initialized(&b[8]);
   memcpy(b, a, itv(0,25));
   r += b[11]; // initialisation unknown
 }
}

void main_local() {
  int* p, *q;
  { int y;
    q = &y;
    memcpy(&p, &q, sizeof(int *));
    q = 0;
  }
  Frama_C_dump_each();
}

void copy_0() {



  int l;
  if (i) memcpy(0, &l, 0);
  if (i) memcpy(&l, 0, 0);
}


void main_all () {
  if (maybe) main (maybe, maybe);
  else if (maybe) main_uninit ();
  else if (maybe) main_local ();
  else if (maybe) copy_0 ();
  while (1); // results of main are unimportant
}
