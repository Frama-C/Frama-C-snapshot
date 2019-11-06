typedef struct Vector {
  int coord[4];
} * vector ;

typedef struct Matrix {
  int coef[4][4];
} * matrix ;

void job( matrix M , vector X , vector R )
{
  int * p = (int *) M->coef ;
  p[14] = 2 ;
  for (int i = 0; i < 4; i++) {
    R->coord[i] = 0 ;
    for (int j = 0; j < 4; i++) {
      vector C = (vector) (M->coef[i]) ;
      R->coord[i] += C->coord[j] * X->coord[j];
    }
  }
}
