/* run.config
   STDOPT: +"-slevel-function" +"init:20" +"-then" +"-report"
*/
#include "share/builtin.h"

extern int b;
extern unsigned int i;

char src[20];
char dst1[20], dst2[20], dst3[20];
char dst4[20];

void init () {
  for (int j=0;j<20;j++) {
    src[j] = j+1;
    dst1[j] = -1;
    dst2[j] = -1;
    dst3[j] = -1;
    dst4[j] = -1;
  }
}


struct t1 { int x; int y; int* p;} v1,v2, v3;
struct t1 t[4];

void main (int a, int b){
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

  //  Frama_C_memcpy(&v2 + (int)&v2, &v1, sizeof(v1));
}
