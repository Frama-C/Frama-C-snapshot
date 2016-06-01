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

void main() {
  main_and_or_rel();
  main_bitwise();
}
