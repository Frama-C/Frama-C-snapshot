/* run.config*
  GCC:
  STDOPT: #"-main g"
*/
int * q;
int x,y=0;
void g(){
  int i = 0;
  if (y==0) i = &y;
  q = (int*)i;
  *q = x;
}
