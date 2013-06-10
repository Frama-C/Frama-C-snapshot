/* run.config
   STDOPT: +"-calldeps" +"-no-deps" +"-slevel-function" +"init:2000" +"-inout-callwise" +"-inout" +"-value-msg-key imprecision" +"-plevel 150" +"-then" +"-report"
*/
#include "share/builtin.h"

extern unsigned int i;
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
  Frama_C_memcpy(p,"d",1);
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
  Frama_C_memcpy(&tm[p],s,4);
  um[0]=0;
  Frama_C_memcpy(&um[p],s,2);

  typ ty = {1, 2};
  ttyp[0] = ty;
  Frama_C_memcpy(&ttyp[p],&ty,sizeof(typ));
}

struct t1 { int x; int y; int* p;} v1,v2, v3, v4, v5;
struct t1 t[4];


void main (int a, int b){
  buggy ();

  many ();

  init ();

  //@ assert 5 <= b && b <= 15;
  Frama_C_memcpy(dst1+1, src+2, b);

  Frama_C_memcpy(dst2+1, src+2, 2*b);

  //@ assert 5 <= b && b <= 14;
  Frama_C_memcpy(dst3+5, src+2, b);

  Frama_C_memcpy(dst4+5, src+2, 2*b);

  v2 = v2;
  v2.p = &v1.y;
  t[1]=v2;

  v1.x = 5;
  v1.y = 7;
  Frama_C_memcpy(&v2, &v1, sizeof(v1));

  Frama_C_memcpy(t+2, t, (1+!a)*sizeof(v1));

  Frama_C_memcpy(&v3, t+(int)t, sizeof(v1));

  Frama_C_memcpy(&v4 + (int)&v4, &v1, sizeof(v1));
  v4.y = &t[0];
  Frama_C_memcpy(&v5 + (int)&v5, &v4, sizeof(v4));

  if (maybe) {
    int x=1;
    while(1)
      Frama_C_memcpy((void *)&x, (void const*)&x, i);
  }  

  char *p;
  p = maybe ? &dst5[0] : &dst5[20];
  Frama_C_memcpy(p, &src[0], b);
  b = maybe;
  //@ assert 1 <= b < 20;
  p = maybe ? &dst5[40] : &dst5[70];
  Frama_C_memcpy(p, &src[0], b);

  // Destination pointer is unbounded
  char ptop1[100];
  int *pptop = ptop1;
  while (1) {
    pptop++;
    if (maybe) break;
  }
  Frama_C_memcpy(pptop, src, 4);

  char ptop2[100];
  pptop = &ptop2[50];
  while (1) {
    pptop--;
    if (maybe) break;
  }
  Frama_C_memcpy(pptop, src+1, 4);

  char ptop3[100];
  pptop = &ptop3[2];
  while (1) {
    if (maybe) pptop--;
    if (maybe) pptop++;
    if (maybe) break;
  }
  Frama_C_memcpy(pptop, src+2, 4);

  char ptop4[100];
  pptop = &ptop4[2];
  while (1) {
    if (maybe) pptop--;
    if (maybe) pptop++;
    if (maybe) break;
  }
  Frama_C_memcpy(pptop, src+2, 5);

  // Size is a garbled mix
  char garbledsize[100];
  int* pgarbledsize = &garbledsize[10];
  Frama_C_memcpy(pgarbledsize, src, garbledsize);
}
