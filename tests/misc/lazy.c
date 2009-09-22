int a=-1;
int b;
void main (int *p) {

  if (p && *p ) *p = 0 ;
  if (&a) { a=0; b=1; }
  if (&a+1) a=1;
  if (&a+2) a=2;
  return;
}
