/* run.config*
   GCC:
   STDOPT: +"-no-input -no-out -deref"
*/
int a,b,c,d,e,*p, t[10];


int main (void)
{
  int i = 0;
  p = &a;
  return *p + b + *(&c) + (&d)[i] + t[i];
}
