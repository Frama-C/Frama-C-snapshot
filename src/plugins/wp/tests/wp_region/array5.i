int job( int * p , int * q )
{
  int s = 0 ;
  q = p ;
  while (!*q) { s+=*p ; p[s]; q++; }
  return s;
}
