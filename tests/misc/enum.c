/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main f
*/
typedef enum counter {ZERO,ONE,TWO};

void f(void)
{
  int i[3]={0};
  enum counter j=0;
  for(j=0;j<2;j++)
    i[j] = 1;

 
}
