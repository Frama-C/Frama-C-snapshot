int A[10] ;
int B[20] ;

int job(int c,int k)
{
  int s = 0 ;
  int * p = (c?A:B)+k ;
  while (!*p) { s += *p; p++; }
  return s ;
}
