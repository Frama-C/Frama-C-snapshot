/* run.config*
   STDOPT: #"-eva -warn-unsigned-overflow"
   STDOPT: #"-no-warn-left-shift-negative"
*/

int a,b,d,e,f,g,h;
unsigned int ua,ub,uc,ud,ue,uf;



char t[10];
volatile v;

int main(int c, int z, int zz) {
  a=5024;
  d = 255;
  f= -255;
  if ((c<=3) && (c>=0)) {
    c = 2*c-1;
    if (v) {
      a = 157 << c;
      /*@ assert c >= 0; */ /* Reduction by the alarm on RHS */ }
    if (v) {
      d=1975;
      d = d >> c;
      /*@ assert c >= 0; */ /* Reduction by the alarm on RHS */ }
    if (v) {
      f= -1975;
      f = f >> c; }
    if (v) {
      c = c << 3;
      /*@ assert c >= 0; */ /* Reduction by the alarm on LHS */ }
    }

  if (z & 1) z=1<<32;
  if (zz) zz=1>>5555;

  if (z & 16) {
    b = 66;
    b = b << b;
    };


  ua = 5607;
  ua >>= 2 ;
  ub = (unsigned int)(-3000);
  ub >>= 2;
  Frama_C_show_each("ua:%u\nub:%u\n",ua,ub);

  if (z & 32)
  {
    int r = (unsigned long)t << 8;
    r += (long)t << 8;
  }

  unsigned int shl = 1;
  if (v) {
    shl = 2U << 31; // "Unsigned overflow."
  }

  return b;
}
