int A[10] ;
int B[20] ;

int job(int k)
{
  int s = 0 ;
  int * p = A+k ;
  while (!*p) { s += *p; p++; }
  return s ;
}
