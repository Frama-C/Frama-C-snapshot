void job( int cols , int rows , int ** m , int * v , int * r )
{
  for (int i = 0; i < rows; i++) {
    r[i] = 0 ;
    for (int j = 0; j < cols; j++)
      r[i] += m[i][j] * v[j] ;
  }
}
