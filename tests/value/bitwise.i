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

void main() {
  main_and_or_rel();
  main_bitwise();
  main_bug1();
  main_bug2();
}
