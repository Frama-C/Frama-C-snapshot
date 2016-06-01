/* run.config*
   COMMENT: this line preserves locations...
*/
int a,b,d,e,i,*p,*q;

void f(int, int*);

void main(int c)
{
  b = 1;
  if (c) p = &a; else p = &b;
  *p = 2;

   a = (int)(&d + 1);

   q = &a;

   L: *((char*)&p+i) = *((char*)&q+i);
   i++;
  if (i<4) goto L;
/*
  *p = (int) &e;

  f(0, &i);

  f(1, &a);

  f(0, &a);
*/
  return;
}

void f(int x, int *r)
{
  a = x;
  (*r)++;
  if (x - a != 0)
    *p = a;

  q = x ? &a : (int*)0;

  //@ assert \valid(q);
  *q = b;
}
