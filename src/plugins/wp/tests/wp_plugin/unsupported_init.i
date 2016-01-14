/*@ requires r1: \initialized(Y+(0 .. 99));
    assigns X[0..99];
    ensures X[0] == Y[0];
*/
void cp( int *X, int *Y );

void f (int *A, int *B) {
  cp(B, A);
  /*@ assert a1: A[0] == B[0]; */
}

