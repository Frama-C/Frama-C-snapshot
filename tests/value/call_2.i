/* run.config*
   STDOPT: +"-warn-signed-overflow"
*/

void ff(float f, int i, int j){
  Frama_C_show_each(i, f, j);
}

void main(int i, int j, int c) {
  float f;
  int z;
  int *p = (int*)&f;
  *p = i;
  if (c) z = 1;
  ff(f, i+j, z); // Arguments with potential RTE
}
