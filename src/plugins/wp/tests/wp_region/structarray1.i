typedef struct Vector {
  int coord[4];
} * vector ;

typedef struct Matrix {
  int coef[4][4];
} * matrix ;

//@ region *X , *R ;
void job( matrix M , vector X , vector R )
{
  for (int i = 0; i < 4; i++) {
    R->coord[i] = 0 ;
    for (int j = 0; j < 4; i++) {
      R->coord[i] += M->coef[i][j] * X->coord[j];
    }
  }
}
