/* run.config
   OPT: -memory-footprint 1 -val -cpp-command "gcc -C -E -DPTEST" -journal-disable
   OPT: -memory-footprint 1 -machdep ppc_32_diab -val -cpp-command "gcc -C -E -DPTEST" -journal-disable
*/

#ifndef PTEST
#include <stdio.h>
#endif

struct s { char c; char a[2]; };

struct s S;

char t[10][10];

int d1,s1,d2,s2;

int main(void)
{
  d1 = (int)&S.a - (int)&S.c;
  s1 = (int)sizeof(struct s);
  d2 = (int)&t[2][2] - (int)&t[0][0];
  s2 = (int)sizeof(t);

#ifndef PTEST
  printf("a-c: %d\nsize: %d\n", d1, s1);
  printf("t[2][2]-t[0][0]: %d\nsize: %d\n", d2, s2);
#endif
  return 0;

}
