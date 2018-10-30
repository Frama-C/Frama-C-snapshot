/* run.config*
   STDOPT: +"-ulevel 22" +"-then -eva-no-warn-pointer-subtraction"
*/


// #include <stdio.h>
int a;
int t[25];
void main1()
{
  int i,j;
  for (i=-10; i< 10; i++)
    {
      t[i+10] = (int*)(i+10)-(int*)10;
//      printf("%d\n",(int*)(i+10)-(int*)10);
      }
  j = -i;
//  printf("%d %d\n",(int)&a,(int)(&a-(int*)0));
}

volatile int vol;

void main2() {
  int x, y;
  int d;
  int *p1 = &x;
  int *p2 = &y;

  if (vol) {
    d = p1 - p2;
    Frama_C_show_each(d);
  }

  p2 = (char*)&x + 3;
  d = p1 - p2;
  Frama_C_show_each(d);

  p2 = (char*)&x - 3;
  d = p1 - p2;
  Frama_C_show_each(d);

  if (vol) {
    p1 = &x+1;
    p2 = &x+7;
  } else {
    p1 = &y+8;
    p2 = &y+19;
  }
  d = p2 - p1;
  Frama_C_show_each(d);
  d = p2 - p2;
  Frama_C_show_each(d);

  p1 = &x + (int)&x;
  p2 = &x;
  d = p2 - p1;
  Frama_C_show_each(d);

  int i = vol;
  //@ assert 0 <= i <= 4;
  p1 = &x + i;
  p2 = p1 + 1;
  d = p2 - p1;
  Frama_C_show_each(d);
  //@ assert d == 1;
}

void main() {
  main1();
  main2();
}
