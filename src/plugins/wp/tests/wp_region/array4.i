int job( int * p )
{
  int s = 0 ;
  int *q = p ;
  while (!*q) { s+=*q ; q++; }
  return s;
}
