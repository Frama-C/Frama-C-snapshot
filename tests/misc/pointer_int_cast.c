/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main g -journal-disable
*/
int * q;
int x,y=0;
void g(){
  int i = 0;
  if (y==0) i = &y;
  q = (int*)i;
  *q = x;
}
