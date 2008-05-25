/* run.config
   OPT: -val -slevel 100 -memory-footprint 1 -cpp-command "gcc -C -E -DPTEST "
*/

#ifndef PTEST
#include <stdio.h>
#endif

#define S 100

char s[S];
int c=0;
int s_int;
int *p_int;

int main(void)
{
  char *p = s;

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
  printf("s_int: %d\n", s_int);
#endif
  /* résultat attendu, avec int 32-bits :
     little endian : s_int = -833811464
     big_endian :    s_int = -1480071902
  */
  return 0;
}
  
