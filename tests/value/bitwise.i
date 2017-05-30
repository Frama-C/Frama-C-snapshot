volatile long v;
  
void main_and_or_rel(void)
{
  long x, r1, r2, r3;

  x = v;
  if ((17 <= x) & (x < 64)) {
    r1 = x;
  }

  x = v;
  //@ assert x >= 20 && x <= 40;
  if ((x <= 23) | (x >= 38)) {
    r2 = x; //Could be improved, but this goes beyond basic backward propagation
  } else {
    r3 = x;
  }

}

void main_bitwise() {
  unsigned int i = 5;
  unsigned int j = ~i;
  int k = ~(int)i;
}

void main_bug1()
{
  unsigned char msb = 3 << 1;
  unsigned char lsb = 3;
  unsigned char par = msb ^ lsb;
  int p1 = (par & 0x0F);
  int p2 = ((int)par >> 4);
  par = (unsigned char)(((int)par & 0x0F) ^ ((int)par >> 4));
}

void main_bug2() {
  int t = v ? 1 : 2;
  if ((t & 7) == 1) { Frama_C_show_each_then(); } else { Frama_C_show_each_else(); }
}

/* See issue Value/Value#82 on the bitwise domain. */
void main_bug3 () {
  unsigned long l_1180 = 10022045811482781039u;
  unsigned long foo = ~ (l_1180 ^ (unsigned long)(l_1180 != 0UL));
  Frama_C_dump_each();
  foo ^= 0;
}

/* Due to signedness mismatches, the bitwise domain incorrectly returned
   Bottom on one of the branches. */
void main_bug4() {
  int g_2 = v ? -1 : 0;
  short tmp = -0x1578;
  if ((g_2 | (int)tmp) & 1) {
    Frama_C_show_each_then();
  } else {
    Frama_C_show_each_else();
  }
}

void main() {
  main_and_or_rel();
  main_bitwise();
  main_bug1();
  main_bug2();
  main_bug3();
  main_bug4();
}
