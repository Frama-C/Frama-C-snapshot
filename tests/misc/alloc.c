/* run.config
   GCC:
   OPT: -val -deps -out -input -journal-disable
   OPT: -val -deps -out -input -absolute-valid-range 0x100-0x200 -main main_abs -journal-disable
*/

#define FRAMA_C_MALLOC_INDIVIDUAL
#include "share/libc/stdlib.c"

int *p,*q,*r,a,b;
char *t,*u,*v;
char ch = 44;

void main(int c, int d, int e)
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
  *r = *(r+1);

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
  *r = r;

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
