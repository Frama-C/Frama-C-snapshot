/* run.config
   STDOPT: 
   STDOPT:  +"-no-collapse-call-cast"
*/

extern int i;

int f () {
  return i;
}

volatile int c;

int g() {
  int x;
  if (c) x = 1;
  return x;
}

char h() {
  return 1;
}

void main () {

  if(c) {float f_ = f();}
  if(c) {long long v = g();}
  if(c) {
    int* x = 0;;
    int **p = &x;
    **p = h();
  }
}

