/* run.config
   OPT: -val -cpp-command "gcc -C -E -DPTEST" -journal-disable
*/

// removed :    OPT: -machdep ppc_32_diab -val -cpp-command "gcc -C -E -DPTEST" -journal-disable

#ifndef PTEST
#include <stdio.h>
#endif

struct s { char c; char a[2]; };

struct s S;

char t[10][10]={0,0,1,1,1,1,1};

int d1,s1,d2,s2,overlapread1, overlapread2,  overlapread3, overlapread4;

int main(void)
{
  d1 = (int)&S.a - (int)&S.c;
  s1 = (int)sizeof(struct s);
  d2 = (int)&t[2][2] - (int)&t[0][0];
  s2 = (int)sizeof(t);
  overlapread1 = *(int*)((int)t + 3);
  overlapread3 = 1 + *(int*)((int)t + 3);
  overlapread2 = *(int*)((int)t + 2);
  overlapread4 = 1 + *(int*)((int)t + 2);
#ifndef PTEST
  printf("a-c: %d\nsize: %d\n", d1, s1);
  printf("t[2][2]-t[0][0]: %d\nsize: %d\n", d2, s2);
#endif
  return 0;

}
