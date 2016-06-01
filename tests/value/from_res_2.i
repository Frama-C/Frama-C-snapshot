typedef unsigned char T;
// typedef int T;
int G;
T f (int left, int right ) { 
  return left + right; 
}

int A, B, C;

struct S { int a; int b; int c;} x1, x2, x3, y1, y2, y3;

struct S g1(void){
  return x1;
}

struct S g2(void){
  return x2;
}

struct S g3(void){
  return x3;
}

void main (void) 
{ int * p = &G; 
  *p = f (G, 3);

  x1.a = A;
  x1.c = C;
  y1 = g1();

  x2.a = A;
  x2.b = B;
  y2 = g2();

  x3.b = B;
  x3.c = C;
  y3 = g3();
 }

