/* run.config
   OPT: -val -slevel 100 -cpp-command "gcc -C -E -DPTEST " -journal-disable -no-warn-signed-overflow
   OPT: -val -slevel 100 -cpp-command "gcc -C -E -DPTEST " -journal-disable -machdep ppc_32 -no-warn-signed-overflow
*/

#ifndef PTEST
#include <stdio.h>
#endif

#define S 100

char s[S];
int c=0;
int s_int;
int *p_int;
char ones[]="11111111"; 
char one23[]="1223";
int col_ones;
int col_123;

int main(void)
{
  char *p = s;

  col_ones = 1 + * (int*) ones;
  col_123 = 1 + * (int*) one23;

  while (p <= s+S-sizeof(int))
    {
      c = 7 * c + 97;
      if (c % 3 == 0)
	*p++ = c;
      else if (c % 3 == 1)
      	{
	  *(short*)p = c;
	  p += sizeof(short);
	}
      else
      	{
	  *(int*)p = c;
	  p += sizeof(int);
	}
    }

  for (p_int = (int*) s; p_int < (int*)(s+S); p_int++)
    {
      s_int = 11 * s_int + *p_int;
    }

#ifndef PTEST
  printf("s_int: %d  col_ones: %d col_123:%d\n", s_int, col_ones, col_123);
#endif
  /* résultat attendu, avec int 32-bits :
     little endian: s_int = -833811464
     big_endian :  s_int: -1480071902  col_ones: 825307442 col_123:825373236
  */
  return 0;
}
