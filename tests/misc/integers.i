int bin,hex,oct,dec;
void main() {
  bin=0b101010 + 0B101010;
  hex=0x2A + 0X2a;
  oct=052 + 0052;
  dec=42;
  /*@ assert bin == hex == oct == dec * 2 == 0b1010100 ; */
}
