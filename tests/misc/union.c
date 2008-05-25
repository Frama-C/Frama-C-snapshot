union { int a; char b;} G;

struct S1 {char a; int b; } v1;

union {struct S1 s1; struct S1 s2;} UU;

void main (void) {
//  G.a = 1;
//  G.b = 2;

//  UU.s1.a = 0;
//  UU.s1.b = 1;
//  *((int*)(&(UU.s1.a))) = 77;

  *((int*)(&(v1.a))) = 77;

}
