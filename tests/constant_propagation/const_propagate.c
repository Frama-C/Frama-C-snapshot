/* run.config
   OPT: -memory-footprint 1 -val -deps -out -input -semantic-const-folding 
*/
int x,y,z;
int TAB[10];

void main(int a) {
  x = 5;
  y = 7;
  z = x+y;
  z = a;
  int *p = &x;
  *p = y;
  int *q = a?p:&y;
  int * r = &TAB[4] ;
  *r = a;
  char *s= ((char *)&x) +1;
  *s = 1;
  
}
