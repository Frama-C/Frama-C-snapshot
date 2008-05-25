#include "share/builtin.h"
struct t1 { int x; int y;} v1,v2;

int main (int a, int b){
  v2 = v2;

  v1.x = 5;
  v1.y = 7;
  Frama_C_memcpy(&v2, &v1, sizeof(v1));

  }
