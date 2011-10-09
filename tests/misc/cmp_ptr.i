/* run.config
   OPT: -memory-footprint 1 -val -deps -out -input -journal-disable 
   OPT: -memory-footprint 1 -val -deps -out -input -journal-disable -undefined-pointer-comparison-propagate-all
*/

int *p,T[10]={0,1,2,3,4,5,6,7,8,9};
char C[10]={0,1,2,3,4,5,6,7,8,9};
char *q;
int f(void) { /* make a top integer */
  int i = 0;
  while (&i+(int)&i) {
    i++;}
  return i;
};

int g(void);

int x,y,z,t,r;
float ff;
int main (int u) {

  p = &T[1] + f();
  q = &C[1] + f();
  
  if (p >= &(T[5])) {*p=88;*q=77;}
  x = !(&y+2);
  *(int*)&ff = &y + 2;
  y = !ff;

  
  z = (u?&f:&g) == 0;
  t = (1 + (int)(u?&f:&g)) == 0;
        
  r = (T-1) == 0;
}

