/* run.config
   OPT: -wp -wp-huge  9 -wp-print
   OPT: -wp -wp-huge 10 -wp-print
   */

int a[5] ;

//@ensures \result >= 0 ;
int f(void)
{
  int k = 0 ;
  int r = 0 ;
  if (a[k++]) r++ ; // 0
  if (a[k++]) r++ ; // 1
  if (a[k++]) r++ ; // 2
  if (a[k++]) r++ ; // 3
  if (a[k++]) r++ ; // 4
  return r ;
}
