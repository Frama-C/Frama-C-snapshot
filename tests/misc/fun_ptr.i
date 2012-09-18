

int f(int x)
{
  return x+1;
}

int g(int x, int y)
{
  return x+y;
}

typedef int (*fptr1)(int);
typedef int (*fptr2)(int, int);
typedef double (*fptr3)(int);

long t[2] = { (long)&f, (long)&g };

int R1, R2;
double R3;

void test1(int nd)
{
  R1 = ((fptr1)(t[nd]))(3);
}

void test2(int nd)
{
  R2 = ((fptr2)(t[nd]))(3, 4);
}

void test3(int nd)
{
  R3 = ((fptr3)(t[nd]))(5);
}

double h(short a, short b) {
  return a + b;
}

volatile int v;

main(int c){
  test1(!(c&1));
  test2(!(c&2));
  if (c&4) test3(!(c&8));
  double (*ph)() = h;
  if (c&16)
    ph(1., 2.);
  if (c&32)
    ph();
  if (c&64)
    ph((short)1, (short)2);

  return 0;
}
