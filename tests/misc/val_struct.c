/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main simple -journal-disable
*/
int simple (int a, int b){
  int w;
  struct t1 { char x; char y;} v1,v2 = {0,1};
  v1=v2;
  v1.x = v2.y;
  return v1.x;
}

#if 0
#include <math.h>   /* pour atan */
#include <complex.h>
int main()
{
  double pi = 4*atan(1);
  complex z = cexp(I*pi);
  printf("%f+%f*i\n", creal(z), cimag(z));
  return (0);
}
#endif
