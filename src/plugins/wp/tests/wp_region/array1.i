//@ region *p, *q ;
int job( int n, int * p , int * q )
{
  int s = 0 ;
  for (int k = 0; k < n; k++)
    s += p[k] * q[k] ;
  return s ;
}
