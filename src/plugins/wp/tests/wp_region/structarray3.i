typedef struct Vector {
  int coord[4];
} * vector ;

typedef struct Matrix {
  int coef[4][4];
} * matrix ;

void job( int c , matrix P , matrix Q , vector X , vector R )
{
  matrix M = c ? P : Q ;
  for (int i = 0; i < 4; i++) {
    R->coord[i] = 0 ;
    for (int j = 0; j < 4; i++) {
      R->coord[i] += M->coef[i][j] * X->coord[j];
    }
  }
}
