/* run.config
   OPT: -check tests/misc/bts0525-2.i
*/
typedef enum {E3=2, E4} T_EN2 ;
typedef enum {E1=2, E2} T_EN1 ;

int f1(T_EN1 p1)
{
  if (p1==E1) return 1;

  return 0;
}
