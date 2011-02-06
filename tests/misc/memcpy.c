#include "share/builtin.h"
struct t1 { int x; int y; int* p;} v1,v2, v3;
struct t1 t[4];

int main (int a, int b){
  v2 = v2;
  v2.p = &v1.y;
  t[1]=v2;
  
  v1.x = 5;
  v1.y = 7;
  Frama_C_memcpy(&v2, &v1, sizeof(v1));

  Frama_C_memcpy(t+2, t, (1+!a)*sizeof(v1));

  Frama_C_memcpy(&v3, t+(int)t, sizeof(v1));

  Frama_C_memcpy(&v2 + (int)&v2, &v1, sizeof(v1));
}
