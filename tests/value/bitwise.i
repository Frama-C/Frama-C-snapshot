/* run.config*
   STDOPT: +"-big-ints-hex 256"
*/

/*@ assigns \result \from min, max;
    ensures min <= \result <= max ;
 */
int Frama_C_interval(int min, int max);


volatile long v;
volatile unsigned char input[3];


extern unsigned short s;

void test1(void) {
  int or1, or2, or3, or4, or5;
  int and1, and2, and3, and4, xor1, xor2;
  unsigned int uand1, uand2, uand3, uand4, uand5;
  int a,b,c,d,e;

  a = Frama_C_interval(3,17);
  b = Frama_C_interval(-3,17);
  c = Frama_C_interval(13,27);
  or1 = a | b;
  or2 = a | c;
  or3 = b | c;

  and1 = a & b;
  and2 = a & c;
  and3 = b & c;

  uand4 = 0xFFFFFFF8U & (unsigned int) c;

  xor1 = a ^ a;
  xor2 = a ^ b;

  unsigned i1 = s * 2;
  unsigned i2 = s * 4;
  unsigned v1 = i1 & i2;
  unsigned v2 = i1 | i2;

  unsigned mask07 = (16 * s + 13) & 0x7;
  unsigned mask0f = (16 * s + 13) & 0xF;
  unsigned mask1f = (16 * s + 13) & 0x1F;
}

void test2(void) {
  int x = Frama_C_interval(62,110) & ~(7);
}

void test3(void) {
  int x = (input[0] & 0x10 ? -1^255 : 0) | input[1];
  int y = (input[0] & 0x20 ? -1^255 : 0) | input[2];
}

int test4(void)
{
  unsigned something = v;
  //@ slevel 2;
  //@ assert something >= 0x80000000 || something < 0x80000000;
  unsigned topBitOnly = something & 0x80000000;
  Frama_C_show_each_1(something,topBitOnly);
  something ^= topBitOnly;
  Frama_C_show_each_2(something,something & 0x80000000,topBitOnly);
  if (something & 0x80000000) {
    Frama_C_show_each_true(something);
    return 0;
  } 
  else {
    Frama_C_show_each_false(something);
    return 1;
  }
}

void and_or_rel(void)
{
  long x, r1, r2, r3;

  x = v;
  if (((17 <= x) & (x < 64))) {
    r1 = x;
  }

  x = v;
  //@ assert x >= 20 && x <= 40;
  if (((x <= 23) | (x >= 38))) {
    r2 = x; //Could be improved, but this goes beyond basic backward propagation
  } else {
    r3 = x;
  }

}

void double_neg() {
  unsigned int i = 5;
  unsigned int j = ~i;
  int k = ~(int)i;
}

void bug1()
{
  unsigned char msb = 3 << 1;
  unsigned char lsb = 3;
  unsigned char par = msb ^ lsb;
  int p1 = (par & 0x0F);
  int p2 = ((int)par >> 4);
  par = (unsigned char)(((int)par & 0x0F) ^ ((int)par >> 4));
}

void bug2() {
  int t = v ? 1 : 2;
  if ((t & 7) == 1) { Frama_C_show_each_then(); } else { Frama_C_show_each_else(); }
}

/* See issue Value/Value#82 on the bitwise domain. */
void bug3 () {
  unsigned long l_1180 = 10022045811482781039u;
  unsigned long foo = ~ (l_1180 ^ (unsigned long)(l_1180 != 0UL));
  Frama_C_show_each(l_1180, foo);
  foo ^= 0;
}

/* Due to signedness mismatches, the bitwise domain incorrectly returned
   Bottom on one of the branches. */
void bug4() {
  int g_2 = v ? -1 : 0;
  short tmp = -0x1578;
  if ((g_2 | (int)tmp) & 1) {
    Frama_C_show_each_then();
  } else {
    Frama_C_show_each_else();
  }
}

/* See issue #639 and merge request #2230 on the bitwise domain. */
void bug5() {
  int x = v;
  x = x | 2;
  if (x == 8) {
    x = x & 2; /* This branch is dead, but the bitwise domain leads to bottom
                  only after the operation x&2 and not before. */
    Frama_C_show_each_dead(x);
  }
}

void main(void) {
  test1();
  test2();
  test3();
  test4();
  and_or_rel();
  double_neg();
  bug1();
  bug2();
  bug3();
  bug4();
  bug5();
}
