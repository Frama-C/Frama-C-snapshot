/* run.config*
   STDOPT: +"-then -eva-widening-delay 4 -eva-widening-period 3"
*/

int T[3] = {3,1,2};
int TT[3][5] = {{3,3,3,0,0}, {1,0,0,0,0}, {2,2,0,0,0}};
int G = 99;
void main() {
  int i,j=77;
  for (i=0 ; i < 3 ; i++) {
    for (j=0; j < T[i]; j++)
      G = 15/(TT[i][j]);};
}
