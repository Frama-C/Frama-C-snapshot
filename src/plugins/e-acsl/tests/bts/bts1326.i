/* run.config
   COMMENT: complex term left-values
*/

typedef int ArrayInt[5];

/*@ ensures
 *AverageAccel == 
 ((*Accel)[4] + (*Accel)[3] + (*Accel)[2] + (*Accel)[1] + (*Accel)[0]) / 5; @*/
void atp_NORMAL_computeAverageAccel(ArrayInt* Accel,int* AverageAccel)
{
  *AverageAccel = 
    ((*Accel)[4] + (*Accel)[3] + (*Accel)[2] + (*Accel)[1] + (*Accel)[0]) / 5;
}

int main(void) {
  ArrayInt Accel = { 1, 2, 3, 4, 5 };
  int av;
  atp_NORMAL_computeAverageAccel(&Accel, &av);
  return 0;
}
