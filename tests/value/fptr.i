/* run.config*
   GCC:
   OPT: -val @VALUECONFIG@ -journal-disable -then -deps -out
   OPT: -val @VALUECONFIG@ -main main_uninit -journal-disable -inout-callwise -then -deps -out
*/
int R=77; volatile int v; int n;

int f(int (ptr(int x)), int i) {
  n=i; R = ptr(1);
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

short retshort(void)
{
  return 12;
}

int retint(void)
{
  return 42;
}

int TA;

void main (int c)
{
  int in, pin;
  short sh, psh;

  if (c&1) in = retshort();
  if (c&2) sh = retint();
  if (c&4) pin = (*((int (*)())retshort))();
  if (c&8) psh = (*((short (*)())retint))();

  int i=0;
  GLOBAL[0] = h;
  GLOBAL[1] = hh;
  for(i=0;i<3;i++) {
    Frama_C_show_each_F(GLOBAL[i]);
    if (v) { G=f(GLOBAL[i], i+1); Frama_C_show_each(i); /* i==2 is impossible */}
  }

  PTR_FCT p = (c&16) ? &h : &hh;
  if (c&32) TA=(*p)(1/(c&64));
}

void main_uninit (int c)
{
  int i=0;
  volatile int j=0;
  GLOBAL[2]=j;
  GLOBAL[0] = h;
  GLOBAL[1] = hh;
  for(i=0;i<3;i++) {
    Frama_C_show_each_F(GLOBAL[i]);
    G=f(GLOBAL[i],i);
  }
}


