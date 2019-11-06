int job( int * p )
{
  int s = 0 ;
  while (!*p) { s+=*p ; p++; }
  return s;
}
