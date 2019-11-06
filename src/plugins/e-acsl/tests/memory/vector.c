/* run.config
   COMMENT: function call + initialized
*/

#include<stdlib.h>

int LAST;

int* new_inversed(int len, int *v) {
  int i, *p;
  //@ assert \valid(v) && \offset(v)+len*sizeof(int) <= \block_length(v);
  p = malloc(sizeof(int)*len);
  for(i=0; i<len; i++)
    p[i] = v[len-i-1];
  return p;
}

int main(void) {
  int x = 3;
  int v1[3]= { 1, 2, x }, *v2;
  //@ assert \valid(&v1[2]);
  LAST = v1[2];
  //@ assert \initialized(v1+2);
  v2 = new_inversed(3, v1);
  LAST = v2[2];
  //@ assert \initialized(v2+2);
  //@ assert LAST == 1;
  free(v2);
  return 0;
}
