/* run.config
   GCC:
   OPT: -val -deps -deref  -journal-disable
*/
int a,b,c,d,e,*p, t[10];


int main (void)
{
  int i = 0;
  p = &a;
  return *p + b + *(&c) + (&d)[i] + t[i];
}
