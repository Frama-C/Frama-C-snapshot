/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out 
   OPT: -memory-footprint 1 -val -deps -out -main main_uninit
   OPT: -memory-footprint 1 -val -deps -out -main main_alarm
*/
int R=77;

int f(int (*ptr(int x))) {
  R = ptr(1);
  return R;
}

int X=77,XH=0,XHH=0;

int h (int y) {X = y; XH= y; return X;} ;
int hh (int y) {X = y+y; XHH = y; return X;} ;

extern int hhh(int y);

typedef int (* PTR_FCT)(int);
typedef PTR_FCT TYPE[10];
TYPE GLOBAL;
int G;
void main0 (int c)
{
  int i=0;
//  i=c?i:i+1;
  GLOBAL[0] = h;
  GLOBAL[1] = hh;
// X=5;
  G=3;
  G=f(c?&h:&hh);
  G=f(&hhh);
}

void main (int c)
{
  int i=0;
  GLOBAL[0] = h;
  GLOBAL[1] = hh;
  for(i=0;i<3;i++) {
    CEA_F(GLOBAL[i]);
    G=f(GLOBAL[i]);
  }
}

void main_uninit (int c)
{
  int i=0;
  volatile int j=0;
  GLOBAL[2]=j;
  GLOBAL[0] = h;
  GLOBAL[1] = hh;
  for(i=0;i<3;i++) {
    CEA_F(GLOBAL[i]);
    G=f(GLOBAL[i]);
  }
}

int main_alarm(int c)
{
  PTR_FCT p = c ? &h : &hh;
  return (*p)(1/c);
}

