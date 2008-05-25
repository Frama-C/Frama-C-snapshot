/*run.config
  DONTRUN: \old not supported yet
*/
/*@ requires \valid_range(p,0,i) ;
  ensures *(\old(p)+i-3) == 78;
*/
void main (int *p,int i) {
  int T[5];
  /*@ assert \valid_range(T,0,4) ;*/
  p++;
  p++;
  p++;
  *(p+i-3) = 78;
}
