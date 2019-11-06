int A[10] ;
int B[20] ;

int job(int k)
{
  int s = 0 ;
  while (!A[k]) { s += A[k]; k++; }
  return s ;
}
