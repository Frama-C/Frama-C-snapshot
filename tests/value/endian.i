/* run.config*
   STDOPT: +"-machdep x86_32"
   STDOPT: +"-machdep ppc_32"
*/


struct S {unsigned char a; unsigned char b; char c; unsigned char d;} v1;

union U {unsigned int full; struct S part;} UU;

union U0 {
   unsigned short f0 ;
   int f1 ;
   int f2 : 5 ;
   unsigned char const f3 ;
};

void main0 (void) {
  unsigned char b0,b1,b2,b3;
  unsigned int f;
  union U data0;

  data0.full = 0xFF030201;
  b0 = data0.part.a + 1 - 1;
  b1 = data0.part.b + 1 - 1;
  b2 = data0.part.c + 1 - 1;
  b3 = data0.part.d + 1 - 1;
  data0.part.a = 0;
  f = data0.full + 1 -1;

  unsigned short G0 ;
  int G1 ;
  int G2;
  unsigned char G3 ;
  union U0 G= {(unsigned short)65532U};

  G0=G.f0;
  G1=G.f1;
  G2=G.f2;
  G3=G.f3;
}


/* A structure with bitfields to access each bit. */
struct bitfield {
  unsigned char bit0: 1;
  unsigned char bit1: 1;
  unsigned char bit2: 1;
  unsigned char bit3: 1;
  unsigned char bit4: 1;
  unsigned char bit5: 1;
  unsigned char bit6: 1;
  unsigned char bit7: 1;
};

/* An unsigned char with direct access to each bit. */
union bitint {
  unsigned char integer;
  struct bitfield bits;
};

volatile int undet;

/* Tests the bitwise interpretation of integers, according to the endianness of
   the machdep : we modify the bits of a bitint union, and then compute the
   value of the corresponding unsigned integer. */
void interpret_bits () {
  union bitint x;
  x.integer = 0;
  x.bits.bit3 = 1;
  /* {8} in little-endian, {16} in big-endian. */
  unsigned char a = x.integer;
  x.integer = (unsigned char) -1;
  x.bits.bit4 = 0;
  /* {239} in little-endian, {247} in big-endian.  */
  unsigned char b = x.integer;
  x.integer = 0;
  if (undet) x.bits.bit7 = 1;
  /* {0; 128} in little-endian, {0; 1} in big-endian. */
  unsigned char c = x.integer;
  x.integer = 0;
  if (undet) x.bits.bit0 = 1;
  /* {0; 1} in little-endian, {0; 128} in big-endian. */
  unsigned char d = x.integer;
  x.integer = (unsigned char) -1;
  if (undet) x.bits.bit2 = 0;
  /* {251; 255} in little-endian, {223; 255} in big-endian. */
  unsigned char e = x.integer;
  x.integer = 0;
  x.bits.bit1 = 1;
  if (undet) x.bits.bit3 = 1;
  if (undet) x.bits.bit4 = 1;
  if (undet) x.bits.bit5 = 1;
  if (undet) x.bits.bit6 = 1;
  /* [2..122]2%8 in little-endian, [64..94]0%2 in  big-endian. */
  unsigned char f = x.integer;
}


void main () {
  main0 ();
  interpret_bits ();
}
