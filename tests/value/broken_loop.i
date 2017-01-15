/* run.config*
  GCC:
  STDOPT: #"-main loop"
*/
int X;

void loop(int d) {

  if(d) ; else ;
  goto L;
  X=0;
  if(d) X=1; else L:;
  X=2;
  return;
}
