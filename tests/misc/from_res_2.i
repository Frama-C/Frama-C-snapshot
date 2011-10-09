typedef unsigned char T;
// typedef int T;
int G;
T f (int left, int right ) { 
  return left + right; 
}

int A, C;

struct S { int a; int b; int c;} x, y;

struct S g(void){
  return x;
}

void main (void) 
{ int * p = &G; 
  *p = f (G, 3);
  x.a = A;
  x.c = C;
  y = g();
 }

