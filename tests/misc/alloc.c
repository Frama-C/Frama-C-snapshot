/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -journal-disable
   OPT: -memory-footprint 1 -val -deps -out -input -absolute-valid-range 0x100-0x200 -main main_abs -journal-disable
*/

#define FRAMA_C_MALLOC_INDIVIDUAL
#include "../../share/malloc.c"

int *p,*q,*r,a,b,c;
char *t;

void main(int c)
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
