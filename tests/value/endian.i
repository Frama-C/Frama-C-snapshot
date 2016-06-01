
struct S {unsigned char a; unsigned char b; char c; unsigned char d;} v1;

union U {unsigned int full; struct S part;} UU;
unsigned char b0,b1,b2,b3;
unsigned int f;


union U0 {
   unsigned short f0 ;
   int f1 ;
   int f2 : 5 ;
   unsigned char const f3 ;
};

unsigned short G0 ;
int G1 ;
int G2;
unsigned char G3 ;
union U0 G={(unsigned short)65532U};



void main (void) {
  union U data0;
  data0.full = 0xFF030201;
  b0 = data0.part.a + 1 - 1;
  b1 = data0.part.b + 1 - 1;
  b2 = data0.part.c + 1 - 1;
  b3 = data0.part.d + 1 - 1;
  data0.part.a = 0;
  f = data0.full + 1 -1;

  G0=G.f0;
  G1=G.f1;
  G2=G.f2;
  G3=G.f3;


}
