/* run.config
   OPT: -val -deps -out -input -journal-disable
   OPT: -val -deps -out -input -journal-disable -undefined-pointer-comparison-propagate-all
*/

int *p,T[10]={0,1,2,3,4,5,6,7,8,9};
char C[10]={0,1,2,3,4,5,6,7,8,9};
char *q;
extern int top_p, top_q;

int f(void);
int g(void);

int x,y,z,t,r;
float ff;
int main (int u) {

  p = &T[1] + top_p;
  q = &C[1] + top_q;
  
  if (p >= &(T[5])) {*p=88;*q=77;}
  x = !(&y+2);
  *(int*)&ff = &y + 2;
  y = !ff;

  
  z = (u?&f:&g) == 0;
  t = (1 + (int)(u?&f:&g)) == 0;
        
  r = (T-1) == 0;
}

