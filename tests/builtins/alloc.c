/* run.config*
   GCC:
   STDOPT: #"-eva-no-builtins-auto"
   STDOPT: #"-eva-no-builtins-auto -absolute-valid-range 0x100-0x200 -main main_abs"
*/

#define malloc(n) Frama_C_malloc_fresh(n)
#include "share/libc/stdlib.c"

int *p,*q,*r,a,b;
char *t,*u,*v;
char ch = 44;

void main(unsigned int c, int d, int e)
{
  p = malloc(4);
  t = malloc (10);
  if (!c++) p[-1] = 0;
  if (!c++) p[1] = 0;
  if (!c++) t[-1] = 0;
  if (!c++) t[10] = 0;
  t[0] = t[9] = 'o';
  *p = 'k';

  q = malloc(4);*q=0;
  r = - (int) q;
  if (!c++) *r = *(r+1); // Invalid No value for r makes r and r+1 valid

  (*q)++;
  a = *q; /* it is incorrect to find 1 here */
  
  u = malloc(!d);
  v = malloc(1 + !d);
  *u = ch;
  *u=33;
  if (e & 1) u[1] = ch;
  if (e & 2) u[1] = 34;
  *v = ch;
  *v=35;
  v[1] = ch;
  v[1]=36;
  if (e & 4) v[2] = ch;
  if (e & 8) v[2] = 37;
}


void main_abs(int c)
{

  q = malloc(4);*q=0;
  r = - (int) q;

  *(int*)0x104=0;
  *r = (int) r;

  (*q)++;
  a = *q; /* it is incorrect to find 1 here */
}

void bug(int c)
{
  int a;
  p = &a;
  if (!c++) p[-1] = 0;
  if (!c++) p[-1] = 0;
  if (!c++) p[-1] = 0;
  if (!c++) p[-1] = 0;
}
