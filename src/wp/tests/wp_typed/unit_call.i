/*@ 
  ensures ( x==1 ==> \result < 0) ;
  ensures ( x==2 ==> \result > 0) ;
*/
int f(int x) ;

void job(void)
{
  int r ;
  r = f(1);
 L1:
  r = f(2);
 L2:
  //@ assert \at(r,L1) != \at(r,L2);
  return ;
}
