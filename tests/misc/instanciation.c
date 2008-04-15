/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -mem-exec f -absolute-valid-range 0x200-0x199 -journal-disable
   OPT: -memory-footprint 1 -val -deps -out -input -mem-exec f -main main1 -absolute-valid-range 0x200-0x199 -journal-disable
   OPT: -memory-footprint 1 -val -main main2 -mem-exec g -absolute-valid-range 0x200-0x199 -journal-disable

*/

int a,b,c,d,*q;
int* p;
int x;

int t[10];

void f(void)
{
  q = p;
  a = *p;
  *p = x;
}

void main(void)
{
  p = &b;
  x = 3;
  f();
  CEA_DUMP();
}

void main1(void)
{
  p = &a;
  x = 3;
  f();
  Frama_C_dump_each();
}

void g(void)
{
  p[1]=3;
  p[2]=4;
}

void main2(void)
{
  p = &(t[1]);
  g();
  CEA_DUMP();
}
