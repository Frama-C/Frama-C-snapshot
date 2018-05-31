      int A[4] = { 1,2,3 } ;
const int B[4] = { 1,2,3 } ;

void validA(void)
{
  //@ assert OK: \valid_read( &B[1] );
  //@ assert OK: \valid( &A[2] );
  A[2] = B[1] ;
}

void validB(void)
{
  //@ assert OK: \valid_read( &A[1] );
  //@ assert KO: \valid( &B[2] );
  *((int*)&B[2]) = A[1] ;
}
