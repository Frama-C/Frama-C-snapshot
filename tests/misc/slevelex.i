/* run.config
   STDOPT: +"-slevel 5 -slevel-function main:0 -slevel-function gu:21 -slevel-function ginc:21"
*/

volatile int c;

int f(void)
{
  int x, y;
  if (c)
    {
      x = 1;
      y = 1;
    }
  else
    {
      x = 2;
      y = 2;
    }
  return x*x - y*y;
}

void gu(int u)
{
      /*@ assert 
      u ==  1 || 
      u ==  2 || u ==  3 ||
      u ==  4 || u ==  5 || 
      u ==  6 || u ==  7 || 
      u ==  8 ||
      u ==  9 || u == 10 ||
      u == 11 || 
      u == 12 || u == 13 ||
      u == 14 || u == 15 || 
      u == 16 || u == 17 || 
      u == 18 ||
      u == 19 || u == 20 ;
    */

  Frama_C_show_each_u(u);
}

void ginc(int u)
{
  int inc;
  inc  = 4 * u;

    /*@ assert 
      inc ==   4 || 
      inc ==   8 || inc == 12 ||
      inc ==  16 || inc == 20 || 
      inc ==  24 || inc == 28 || 
      inc ==  32 ||
      inc ==  36 || 
      inc ==  40 || inc == 44 ||
      inc ==  48 || inc == 52 || 
      inc ==  56 || inc == 60 || 
      inc ==  64 || inc == 68 || 
      inc ==  72 || inc == 76 ||
      inc == 80 ; 
    */

  Frama_C_show_each_inc(inc);
}


void main(int un)
{
  int x, y;
  if (c)
    {
      x = 1;
      y = 1;
    }
  else
    {
      x = 2;
      y = 2;
    }
  //@ assert x*x == y*y ;

  Frama_C_show_each_xy(x,y);
  x = f();
  //@ assert x == 0;

  if (un>=20) un = 20;
  if (un<=1) un = 1;
  gu(un);
  ginc(un);
}
