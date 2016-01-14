
/*@ 
  requires \base_addr(a) == \base_addr(b) ;
  ensures \subset(a , b + (0..n)) <==> \result ;
*/
int mem(int * a , int *b , int n)
{
  return (b <= a) && (a <= b + n) ;
}
