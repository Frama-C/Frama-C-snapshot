
struct S {unsigned char a; unsigned char b; char c; unsigned char d;} v1;

union U {unsigned int full; struct S part;} UU;
unsigned char b0,b1,b2,b3;
unsigned int f;
void main (void) {
  union U data0;
  data0.full = 0xFF030201;
  b0 = data0.part.a + 1 - 1;
  b1 = data0.part.b + 1 - 1;
  b2 = data0.part.c + 1 - 1;
  b3 = data0.part.d + 1 - 1;
  data0.part.a = 0;
  f = data0.full + 1 -1;

}
